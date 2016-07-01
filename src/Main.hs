{-# LANGUAGE TemplateHaskell #-}

module Main where
  
import Data.Time.Clock.POSIX
import Data.Time
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Random
import Control.Lens
import Numeric
import Linear.V2
import Linear.V4
import Linear.Affine (Point(P))

import SDL (($=))
import qualified SDL
import qualified SDL.Font
import qualified Data.Text as T

import Game

  
getInput :: IO GameInput
getInput =  do
  events <- map SDL.eventPayload <$> SDL.pollEvents

  let eventToGI :: SDL.EventPayload -> Last GameInput
      eventToGI (SDL.KeyboardEvent e)
        | SDL.keyboardEventKeyMotion e == SDL.Pressed = 
            case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
              SDL.KeycodeW -> Last (Just $ MoveTo UP)
              SDL.KeycodeS -> Last (Just $ MoveTo DOWN)
              SDL.KeycodeA -> Last (Just $ MoveTo LEFT)
              SDL.KeycodeD -> Last (Just $ MoveTo RIGHT)
              SDL.KeycodeUp -> Last (Just $ MoveTo UP)
              SDL.KeycodeDown -> Last (Just $ MoveTo DOWN)
              SDL.KeycodeLeft -> Last (Just $ MoveTo LEFT)
              SDL.KeycodeRight -> Last (Just $ MoveTo RIGHT)
              SDL.KeycodeR -> Last (Just Restart)
              SDL.KeycodeP -> Last (Just Pause)
              SDL.KeycodeQ -> Last (Just Quit)
              _ -> mempty
        | otherwise = mempty
      eventToGI SDL.QuitEvent = Last (Just Quit)
      eventToGI _ = mempty

      gameInp = fromMaybe None . getLast . foldMap eventToGI $ events

  return gameInp

draw :: RenderContext -> GameSetup -> GameData -> IO ()
draw rc@(renderer, _) setup gd = do
  SDL.renderDrawColor renderer $= V4 0 0 0 0
  SDL.renderClear renderer

  drawGame rc setup gd

  SDL.renderPresent renderer
  

appTitle = "Snake Game"

screenWidth = 640 :: Int
screenHeight = 360 :: Int

fieldOffset = V2 10 10

cellSize = 20 :: Int
segmentSize =  15 :: V2 Int
foodSize =  12 :: V2 Int

textBarOffset = V2 360 0 :: V2 Int
titleTextPos = V2 0 0 :: V2 Int        
        
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  
  window <- SDL.createWindow 
              (T.pack appTitle) 
              SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <- SDL.createRenderer
                window
                (-1)
                SDL.RendererConfig
                  { SDL.rendererType = SDL.AcceleratedRenderer
                  , SDL.rendererTargetTexture = False
                  }  

  sprites <- loadSprites renderer


  g <- getStdGen
  time <- getPOSIXTime
  let gameSetup = GameSetup {
    _fieldSize = V2 15 15,
    _foodNum = 5,
    _tickTime = 0.5,
    _initialData = newGame g time
  }
  gameLoop  gameSetup getInput (draw (renderer, sprites) gameSetup)

  mapM_
    (SDL.destroyTexture . sprTexture)
    sprites

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

data SpriteId = 
    SnakeHead
  | SnakeTail
  | Border
  | Food
  | EmptyCell
  deriving (Show, Eq, Enum, Bounded)

data Sprite = Sprite {
    sprId :: SpriteId
  , sprTexture :: SDL.Texture
--  , sprRectangle :: SDL.Rectangle Float
}

type RenderContext = (SDL.Renderer, [Sprite])

spritePath :: SpriteId -> String
spritePath s = case s of
                 SnakeHead -> "data/snakehead.bmp"
                 SnakeTail -> "data/snaketail.bmp"
                 Food      -> "data/food.bmp"
                 _ -> "data/border.bmp"

loadSprites :: SDL.Renderer -> IO [Sprite]
loadSprites renderer = 
  let
    load :: SpriteId -> IO Sprite
    load s = do
      surface <- SDL.loadBMP $ spritePath s
      texture <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      return $ Sprite s texture

  in mapM load [minBound..maxBound]

drawSprite :: RenderContext -> SpriteId -> V2 Int -> V2 Int -> IO ()
drawSprite (renderer, sprites) sId pos size = do
  let spriteRect = SDL.Rectangle
                     (P $ fromIntegral <$> pos)
                     (fromIntegral <$> size)
      (Just sprite) = find (\s -> sprId s == sId) sprites
  SDL.renderCopy
    renderer
    (sprTexture sprite)
    Nothing
    (Just spriteRect)


drawGame :: RenderContext -> GameSetup -> GameData -> IO ()
drawGame rc setup gdata  = do
  let GameData {_snake = snk, _score = scr, _food = fd, _cond = cnd} = gdata
      GameSetup { _fieldSize = fsize} = setup
  drawField rc fsize
  drawSnake rc snk
  drawFood rc fd
  drawTextInfo rc gdata cnd

toScrn :: V2 Int -> V2 Int
toScrn pos = let toScreenSpace = (* cellSize)
                 V2 x y = toScreenSpace <$> pos
                 V2 dx dy = fieldOffset
                 ssPos = V2 (x + dx) (screenHeight - y - cellSize - dy)
             in ssPos 


drawField :: RenderContext -> V2 Int -> IO ()
drawField rc (V2 fieldWidth fieldHeight) = do
  let bricks = [V2 x y | x <- [0..(fieldWidth+1)], y <- [0..(fieldHeight+1)], x == fieldWidth+1 || x == 0 || y == 0 || y == fieldHeight +1]
  mapM_
    (\pos -> drawSprite rc Border (toScrn pos) (V2 cellSize cellSize))
    bricks

drawSnake :: RenderContext -> Snake -> IO ()
drawSnake rc Snake{_body = snk} = do
  mapM_
    (\pos -> drawSprite rc SnakeTail (toScrn pos) segmentSize)
    (tail snk)
  drawSprite rc SnakeHead (toScrn $ head snk) segmentSize

drawFood :: RenderContext -> [Pos2i] -> IO ()
drawFood rc fd = do          
  mapM_
    (\pos -> drawSprite rc Food (toScrn pos) foodSize)
    fd


drawText :: RenderContext -> T.Text -> Int -> V2 Int -> V4 Int -> IO ()
drawText (rnr, _) txt size pos  color = do
  font <- SDL.Font.load "data/half_bold_pixel.ttf" (fromIntegral size)
  textSurf <- SDL.Font.solid font (fromIntegral <$> color) txt
  text <- SDL.createTextureFromSurface rnr textSurf
  textSize <- SDL.surfaceDimensions textSurf

  SDL.renderCopy
    rnr
    text
    Nothing
    (Just $ SDL.Rectangle (P $ fromIntegral <$> pos) (fromIntegral <$> textSize))

  SDL.freeSurface textSurf
  SDL.destroyTexture text
  SDL.Font.free font

speedColor :: Double -> V4 Int
speedColor c 
  | c < 2 = V4 0 255 0 0
  | c >= 2 && c < 3.0 = V4 255 255 0 0
  | c >= 3.0 = V4 255 0 0 0 

drawTextInfo :: RenderContext -> GameData -> GameCondition -> IO ()
drawTextInfo rc@(rnr,_) gdata cnd = do
  let GameData {_score = score, _speed = speed} = gdata
  
  drawText rc (T.pack "Snake!")                55 (V2 360 0) (V4 0 255 0 0)

  drawText rc (T.pack "WASD/Arrows - Control") 20 (V2 360 65) (V4 128 128 128 0)
  drawText rc (T.pack "P - Pause")             20 (V2 360 85) (V4 128 128 128 0)
  drawText rc (T.pack "R - Restart")           20 (V2 360 105) (V4 128 128 128 0)
  drawText rc (T.pack "Q - Quit")              20 (V2 360 125) (V4 128 128 128 0)

  drawText rc (T.pack "Score: ")    25 (V2 360 150) (V4 0 0 255 0)
  drawText rc (T.pack $ show score) 25 (V2 460 150) (V4 255 255 0 0) 
  drawText rc (T.pack "Speed: ")           25 (V2 360 175) (V4 0 0 255 0)
  drawText rc (T.pack $ showFFloat (Just 1) speed "x") 25 (V2 460 175) $ speedColor speed


  case cnd of
    WIN -> drawText rc (T.pack "YOU WIN!!!") 35 (V2 80 150) (V4 255 0 0 0)
    LOSE -> drawText rc (T.pack "YOU LOSE!") 35 (V2 80 150) (V4 255 0 0 0)
    PAUSE -> drawText rc (T.pack "PAUSE") 35 (V2 80 150) (V4 255 0 0 0)
    _ -> return ()

