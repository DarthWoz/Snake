{-# LANGUAGE TemplateHaskell #-}

module Game (
    Game       (..)
  , GameData   (..)
  , GameSetup  (..)
  , GameInput  (..)
  , GameCondition (..)
  , Pos2i      (..)
  , Snake      (..)
  , MoveDir    (..)
  , Time       (..)

  , gameLoop, newGame 
  
  ) where

import Data.List
import Data.Time.Clock.POSIX
import Data.Time
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Linear.V2
import System.Random

type Pos2i = V2 Int
type Time = POSIXTime

data GameCondition = 
    PLAY 
  | WIN 
  | LOSE 
  | ENDGAME 
  | PAUSE
  deriving (Show, Eq)

data MoveDir = 
    LEFT 
  | RIGHT 
  | DOWN 
  | UP
  deriving (Show, Eq)

data GameData = GameData {
    _snake :: Snake
  , _score :: Int
  , _speed :: Double
  , _food :: [Pos2i]
  , _cond :: GameCondition
  
  , _rnd :: StdGen
  , _lastTickTime :: Time
} deriving (Show)

data Snake = Snake {
    _moveDir :: MoveDir
  , _body :: [Pos2i]
} deriving (Show, Eq)

data GameSetup = GameSetup {
    _fieldSize :: V2 Int
  , _foodNum :: Int
  , _tickTime :: Time
  , _initialData :: GameData
} deriving (Show)

makeLenses ''GameData
makeLenses ''Snake
makeLenses ''GameSetup
--initialData :: Lens' GameSetup GameData
--initialData = lens _initialData (\setup idata -> setup {_initialData = idata})

data GameInput = 
    MoveTo MoveDir
  | Pause 
  | Restart 
  | Quit 
  | None
  deriving (Show, Eq)
  
type Game = ReaderT GameSetup (StateT GameData Identity)


defaultSnake = Snake {_body = [V2 5 4, V2 4 4, V2 3 4, V2 2 4], _moveDir = RIGHT}

newGame :: StdGen -> Time -> GameData
newGame g time =
  GameData {
    _snake = defaultSnake,
    _score = 0,
    _speed = 1.0,
    _food = [],
    _cond = PLAY,
    _rnd = g,
    _lastTickTime = time
  }

runGame :: GameSetup -> GameData -> Game a -> GameData
runGame setup gd game = snd . runIdentity $ runStateT (runReaderT game setup) gd

gameLoop :: GameSetup ->
            IO GameInput ->
            --(Time -> GameInput -> Game()) ->
            (GameData -> IO ()) -> 
            IO GameData
gameLoop setup input present = do
  let initGD = _initialData setup
      loop gd = do
        gi <- input
        time <- getPOSIXTime
        let
          newGD = runGame setup gd (gamePlay time gi)
        present newGD        
        case _cond newGD of
          ENDGAME -> return newGD
          _ -> loop newGD
      
  loop initGD  

gamePlay :: Time -> GameInput -> Game ()
gamePlay curTime gi = do
  processInput gi
  gameState <- use cond
  game <- get
  tickTime <-view tickTime
  let dt = curTime - game^.lastTickTime 
  
  case gameState of
    WIN -> return ()
    LOSE -> return ()
    PAUSE ->
      lastTickTime += dt
    PLAY ->
      when (dt >= tickTime / realToFrac (game^.speed)) $ do
        gameTick
        lastTickTime .= curTime  
    _ -> return ()
  
       
processInput :: GameInput -> Game ()
processInput gi =
  case gi of
    (MoveTo mdir) -> do
      snk <- use snake
      snake.moveDir .= validateMoveDir snk mdir
    Pause -> do
      gameState <- use cond
      case gameState of 
        PLAY -> cond .= PAUSE
        PAUSE -> cond .= PLAY
        _ -> return ()
    Restart -> do
      game <- view initialData
      g <- use rnd
      time <- use lastTickTime
      put $ game {_rnd = g, _lastTickTime = time}
    Quit -> 
      cond .= ENDGAME
    None -> return ()
    
validateMoveDir :: Snake -> MoveDir -> MoveDir
validateMoveDir snk mdir = 
  let snkHead = head $ snk^.body
      sndSeg = head . tail $ snk^.body      
  in 
    if snkHead `deposeTo` mdir /= sndSeg
      then mdir
      else snk^.moveDir
      
gameTick :: Game ()
gameTick = do  
  moveSnake
  eatFood
  placeFood
  incSpeed
  checkGameState
 
  --(score .=) =<< use (snake.body.to length)


snakeHead :: Getter GameData Pos2i
snakeHead = snake.body.to head

moveSnake :: Game ()
moveSnake = do
  snkHead <- use snakeHead
  snkMoveDir <- use $ snake.moveDir
  
  let newHeadPos = snkHead `deposeTo` snkMoveDir
  
  snake.body %= (init . (newHeadPos : ))
  
deposeTo :: Pos2i -> MoveDir -> Pos2i
deposeTo pos LEFT = (+) pos $ V2 (-1) 0
deposeTo pos RIGHT= (+) pos $ V2 1 0
deposeTo pos UP   = (+) pos $ V2 0 1
deposeTo pos DOWN = (+) pos $ V2 0 (-1)
  
eatFood :: Game ()
eatFood = do
  foodList <- use food
  snkHead <- use snakeHead
  
  when (snkHead `elem` foodList) $ do
    food %= delete snkHead
    snake.body %= \snk -> snk ++ [last snk]
    
    score %= (+1)
    
placeFood :: Game ()
placeFood = do
  snk <- use $ snake.body
  foodList <- use food
  foodNum <- view foodNum
  (V2 fieldWidth fieldHeight) <- view fieldSize
  let isFree (x, y) = V2 x y `notElem` snk &&
                      V2 x y `notElem` foodList
      freeSells = [V2 x y | x <- [1..fieldWidth], y <- [1..fieldHeight], isFree (x, y)]
      
      fnum = min (foodNum - length foodList) (length freeSells)
      
      randPos = do
        i <- rand (0, length freeSells - 1)
        return (freeSells !! i)        
  
  when (fnum > 0) $ do    
    --newFood <- replicateM fnum randPos
    --food %= (++ newFood)
    newFood <- randPos
    food %= (newFood : )
    

rand :: (Int, Int) -> Game Int
rand r = do
  g <- use rnd
  let (i, g') = randomR r g
  rnd .= g'
  return i
  
incSpeed :: Game ()
incSpeed = do
  scr <- use score
  speed %= \s -> 
    if s >= 4.0 then 4.0 else 1.0 +  0.1 * realToFrac (scr `div` 5)


checkGameState :: Game ()
checkGameState = do
  lose <- isLose
  win <- isWin
  
  let newState = case (win, lose) of
        (True,_) -> WIN
        (_,True) -> LOSE
        _        -> PLAY
  
  cond .= newState
   
  
isLose :: Game Bool
isLose = do
  snkHead@(V2 hX hY) <- use snakeHead
  snkTail <- use $ snake.body.to tail
  (V2 fieldWidth fieldHeight) <- view fieldSize
  return $ snkHead `elem` snkTail 
    || hX == 0 
    || hY == 0 
    || hX == fieldWidth + 1 
    || hY == fieldHeight + 1
  
  
isWin :: Game Bool
isWin = do
  snk <- use $ snake.body
  (V2 fieldWidth fieldHeight) <- view fieldSize
  return $ length snk == fieldHeight * fieldWidth
