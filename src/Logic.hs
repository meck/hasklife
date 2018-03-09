module Logic
  ( Cord
  , Grid
  , evolveGrid
  , gridToList
  , toggleCord
  , randomGrid
  , rPentomino
  ) where

import Data.List.Split (chunksOf)
import qualified Data.Set as S
import System.Random
import Control.Monad

type Cord = (Int, Int)

type Grid = S.Set Cord

-- Returns the (Alive, Dead) neighbours
neigh :: Grid -> Cord -> ([Cord], [Cord])
neigh g (x, y) = (filter (`S.member` g) n, filter (`S.notMember` g) n)
 where
  n =
    [ (x', y')
    | x' <- (x +) <$> [-1 .. 1]
    , y' <- (y +) <$> [-1 .. 1]
    , not (x' == x && y' == y)
    ]

evolveGrid :: Grid -> Grid
evolveGrid g = pruned `S.union` birthed
 where
  nAlive   = length . fst . neigh g
  dead     = snd . neigh g
  posBirth = S.foldr' (S.union . S.fromList . dead) S.empty g
  birthed  = S.filter ((== 3) . nAlive) posBirth
  pruned   = S.filter (\c -> nAlive c > 1 && nAlive c < 4) g

gridToList :: Grid -> [Cord]
gridToList = S.toList

toggleCord :: Cord -> Grid -> Grid
toggleCord c g = if S.member c g then S.delete c g else S.insert c g

-- height of the curve, center of the curve, shape of the curve
gaussDist :: Float -> Float -> Float -> Float -> Float
gaussDist h n q x = h * exp ((x - n) * (x - n) / ((-2) * q * q))

-- Returns a randomized grid normaly distrubuted between the cords
randomGrid :: Cord -> Cord -> IO Grid
randomGrid (xMin, yMin) (xMax, yMax) = do
  let allCords = [ (x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax] ]
      xMin'    = fromIntegral xMin
      yMin'    = fromIntegral yMin
      width    = fromIntegral xMax - xMin'
      hight    = fromIntegral yMax - yMin'
      xProb    = gaussDist 1 (xMin' + width / 2) $ width / 3
      yProb    = gaussDist 1 (yMin' + hight / 2) $ hight / 3
      xyProb (c@(x, y), prob) =
        (c, prob * xProb (fromIntegral x) * yProb (fromIntegral y))
  bs <- replicateM (length allCords) (randomIO :: IO Float)
  return
    $   S.fromList
    $   fmap fst
    $   filter ((> 0.5) . snd)
    $   xyProb
    <$> zip allCords bs

--Example grid
rPentomino :: Grid
rPentomino = S.fromList [(-1, 0), (0, 1), (0, 0), (0, -1), (1, 1)]
