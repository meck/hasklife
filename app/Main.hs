module Main where

import Logic

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

iniWidth, iniHeight, offset :: Int
iniWidth = 1000

iniHeight = iniWidth `div` 2

offset = 30

cellsize :: Int
cellsize = 7

cellsizeF :: Float
cellsizeF = fromIntegral cellsize

stepsPerSec :: Int
stepsPerSec = 8

background :: Color
background = makeColorI 46 52 64 255

cellColor :: Color
cellColor = makeColorI 129 161 193 255

textColor :: Color
textColor = makeColorI 216 222 233 255

textScale :: Float
textScale = 0.2

data World = World
  { grid :: Grid
  , itter :: Int
  , paused :: Bool
  , winW :: Int
  , winH :: Int
  }

initialWorld :: World
initialWorld = World
  { grid   = rPentomino
  , itter  = 0
  , paused = True
  , winW   = iniWidth
  , winH   = iniHeight
  }

window :: Display
window = InWindow "Hasklife" (iniWidth, iniHeight) (offset, offset)

gridToPicture :: Grid -> Picture
gridToPicture g = pictures $ celltoPict <$> gridToList g
 where
  celltoPict (x, y) =
    translate (fromIntegral $ x * cellsize) (fromIntegral $ y * cellsize)
      $ color cellColor
      $ polygon
      $ rectanglePath cellsizeF cellsizeF

worldToPicture :: World -> Picture
worldToPicture w = pictures [it, gridToPicture $ grid w]
 where
  it =
    translate (10 + fromIntegral (winW w `div` (-2)))
              ((-10) + fromIntegral (winH w `div` 2) - (100 * textScale))
      $  scale textScale textScale
      $  color textColor
      $  text
      $  "Iterations: "
      ++ show (itter w)

stepWorld :: Float -> World -> World
stepWorld _ w = if paused w
  then w
  else w { grid = evolveGrid $ grid w, itter = succ $ itter w }

handleInput :: Event -> World -> World
-- Pause Game with Space
handleInput (EventKey (SpecialKey KeySpace) Down _ _) w =
  w { paused = not (paused w) }
-- Toggle cell with left mouse click
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) w = if paused w
  then w { grid = toggleCord (scalePix x, scalePix y) (grid w) }
  else w
  where scalePix x = truncate $ signum x * (abs x + cellsizeF / 2) / cellsizeF
-- Update the windowsize
handleInput (EventResize (x, y)) w = w { winW = x, winH = y }
-- All other input
handleInput _                    w = w

main :: IO ()
main = play window
            background
            stepsPerSec
            initialWorld
            worldToPicture
            handleInput
            stepWorld
