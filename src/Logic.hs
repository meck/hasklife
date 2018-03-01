module Logic
  ( Cord
  , Grid
  , evolveGrid
  , gridToList
  , toggleCord
  , rPentomino
  ) where

import Data.List.Split (chunksOf)
import qualified Data.Set as S

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

--Example grid
rPentomino :: Grid
rPentomino = S.fromList [(-1, 0), (0, 1), (0, 0), (0, -1), (1, 1)]
