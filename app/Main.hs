module Main where

import           Logic

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

iniWidth, iniHeight, offset :: Int
iniWidth = 1000

iniHeight = iniWidth `div` 2

offset = 30

cellsize :: Int
cellsize = 3

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
  { grid   :: Grid
  , itter  :: Int
  , paused :: Bool
  , winW   :: Int
  , winH   :: Int
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

worldToPicture :: World -> IO Picture
worldToPicture w = return $ pictures [it, gridToPicture $ grid w]
 where
  it =
    translate (10 + fromIntegral (winW w `div` (-2)))
              ((-10) + fromIntegral (winH w `div` 2) - (100 * textScale))
      $  scale textScale textScale
      $  color textColor
      $  text
      $  "Iterations: "
      ++ show (itter w)

stepWorld :: Float -> World -> IO World
stepWorld _ w = if paused w
  then return w
  else return $ w { grid = evolveGrid $ grid w, itter = succ $ itter w }

handleInput :: Event -> World -> IO World
handleInput event w = case event of
-- Pause Game with Space
  (EventKey (SpecialKey KeySpace) Down _ _) ->
    return $ w { paused = not (paused w) }
-- Toggle cell with left mouse click
  (EventKey (MouseButton LeftButton) Down _ (x, y)) -> if paused w
    then return $ w { grid = toggleCord (scalePix x, scalePix y) (grid w) }
    else return w
-- Randomize the grid with r
  (EventKey (Char r) Down _ _) -> if paused w
    then do
      let wid = div iniWidth $ 2 * cellsize
          hig = div iniHeight $ 2 * cellsize
      ng <- randomGrid (negate wid, negate hig) (wid, hig)
      return w { grid = ng, itter = 0 }
    else return w
-- Update the windowsize
  (EventResize (x, y)) -> return $ w { winW = x, winH = y }
-- All other input
  _                    -> return w
  where scalePix x = truncate $ signum x * (abs x + cellsizeF / 2) / cellsizeF

main :: IO ()
main = playIO window
              background
              stepsPerSec
              initialWorld
              worldToPicture
              handleInput
              stepWorld
