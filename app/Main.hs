module Main where

import           Data.List                        (mapAccumL)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Logic

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

helpColor :: Color
helpColor = makeColorI 235 203 139 255
textScale :: Float
textScale = 0.2

textScaleSmall :: Float
textScaleSmall = 0.15

data World = World
  { grid     :: Grid
  , itter    :: Int
  , paused   :: Bool
  , winW     :: Int
  , winH     :: Int
  , showHelp :: Bool
  }

initialWorld :: World
initialWorld = World
  { grid     = rPentomino
  , itter    = 0
  , paused   = True
  , winW     = iniWidth
  , winH     = iniHeight
  , showHelp = False
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
worldToPicture w = return $ pictures
  [ if showHelp w then helpPicture (winW w) (winH w) else blank
  , itterPicture (winW w) (winH w) (itter w)
  , gridToPicture $ grid w
  ]

itterPicture :: Int -> Int -> Int -> Picture
itterPicture width hight i =
  translate (10 + fromIntegral (width `div` (-2)))
            ((-10) + fromIntegral (hight `div` 2) - (100 * textScale))
    $  scale textScale textScale
    $  color textColor
    $  text
    $  "Iterations: "
    ++ show i

helpPicture :: Int -> Int -> Picture
helpPicture width height =
  translate
      (10 + fromIntegral (width `div` (-2)))
      ( (-20 - 100 * textScale)
      + fromIntegral (height `div` 2)
      - (100 * textScale)
      )
    $ color helpColor
    $ pictures
    $ snd
    $ mapAccumL
        go
        0
        [ "Space to Pause"
        , "When Paused:"
        , " C: clear screen"
        , " R: randomize"
        , " Click: Add or remove Cell"
        ]
 where
  go acc l =
    ( acc - 150 * textScaleSmall
    , translate 0 acc $ scale textScaleSmall textScaleSmall $ text l
    )

stepWorld :: Float -> World -> IO World
stepWorld _ w = if paused w
  then return w
  else return $ w { grid = evolveGrid $ grid w, itter = succ $ itter w }

-- Pause Game with Space
-- Show help with 'h'
-- Toggle cell with left mouse click
-- Clear with 'c'
-- Randomize the grid with 'r'
handleInput :: Event -> World -> IO World
handleInput event w = case event of
  (EventKey (SpecialKey KeySpace) Down _ _) ->
    return $ w { paused = not (paused w) }
  (EventKey (MouseButton LeftButton) Down _ (x, y)) -> if paused w
    then return $ w { grid = toggleCord (scalePix x, scalePix y) (grid w) }
    else return w
  (EventKey (Char c) Down _ _) -> case c of
    'c' ->
      if paused w then return $ w { grid = emptyGrid, itter = 0 } else return w
    'r' -> if paused w
      then do
        let wid = div iniWidth $ 2 * cellsize
            hig = div iniHeight $ 2 * cellsize
        ng <- randomGrid (negate wid, negate hig) (wid, hig)
        return w { grid = ng, itter = 0 }
      else return w
    'h' -> return $ w { showHelp = not $ showHelp w }
    _   -> return w
  (EventResize (x, y)) -> return $ w { winW = x, winH = y }
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
