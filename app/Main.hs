module Main where

import           Data.List                        (mapAccumL)
import           Data.Semigroup                   ((<>))
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Logic
import           Options.Applicative

windowOffset :: Int
windowOffset = 30

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
  , cellsize :: Int
  , showHelp :: Bool
  }

gridToPicture :: Int -> Grid -> Picture
gridToPicture cs g = pictures $ celltoPict <$> gridToList g
 where
  celltoPict (x, y) =
    translate (fromIntegral $ x * cs) (fromIntegral $ y * cs)
      $ color cellColor
      $ polygon
      $ rectanglePath (fromIntegral cs) (fromIntegral cs)

worldToPicture :: World -> IO Picture
worldToPicture w = return $ pictures
  [ if showHelp w then helpPicture (winW w) (winH w) else blank
  , itterPicture (winW w) (winH w) (itter w)
  , gridToPicture (cellsize w) (grid w)
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
        [ "H: Hide/Show help"
        , "Space: Unpause/Pause"
        , "When Paused"
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
        let wid = div (winW w) $ 2 * cellsize w
            hig = div (winH w) $ 2 * cellsize w
        ng <- randomGrid (negate wid, negate hig) (wid, hig)
        return w { grid = ng, itter = 0 }
      else return w
    'h' -> return $ w { showHelp = not $ showHelp w }
    _   -> return w
  (EventResize (x, y)) -> return $ w { winW = x, winH = y }
  _                    -> return w
 where
  scalePix x =
    truncate $ signum x * (abs x + fromIntegral (cellsize w) / 2) / fromIntegral
      (cellsize w)

data Opt = Opt
  { hSize :: Int
  , wSize :: Int
  , cSize :: Int
  }

pOpt :: Parser Opt
pOpt =
  Opt
    <$> option
          auto
          (  long "height"
          <> short 'h'
          <> help "Vertical window size"
          <> showDefault
          <> value 500
          <> metavar "INT"
          )
    <*> option
          auto
          (  long "width"
          <> short 'w'
          <> help "Horisontal window size"
          <> showDefault
          <> value 1000
          <> metavar "INT"
          )
    <*> option
          auto
          (  long "cellsize"
          <> short 'c'
          <> help "Size of the cells in pixels"
          <> showDefault
          <> value 3
          <> metavar "INT"
          )

main :: IO ()
main = runWorld =<< execParser opts
 where
  opts = info
    (pOpt <**> helper)
    ( fullDesc <> progDesc "Conways game of life in a OpenGL window" <> header
      "Haskelife"
    )

runWorld :: Opt -> IO ()
runWorld opt = playIO window
                      background
                      stepsPerSec
                      initialWorld
                      worldToPicture
                      handleInput
                      stepWorld
 where
  window =
    InWindow "Hasklife" (wSize opt, hSize opt) (windowOffset, windowOffset)
  initialWorld = World
    { grid     = rPentomino
    , itter    = 0
    , paused   = True
    , winW     = wSize opt
    , winH     = hSize opt
    , cellsize = cSize opt
    , showHelp = True
    }
