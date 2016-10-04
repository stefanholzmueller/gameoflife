module Life where

import Prelude
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub, union)

data Cell = Cell { x :: Int, y :: Int }
instance eqCell :: Eq Cell where
  eq (Cell c1) (Cell c2) = c1.x == c2.x && c1.y == c2.y

cell :: Int -> Int -> Cell
cell x y = Cell { x: x, y: y }

neighbors :: Cell -> Array Cell
neighbors (Cell { x, y }) = map (\d -> cell (d.dx + x) (d.dy + y)) directions
  where
    deltas = [ -1, 0, 1 ]
    directions = do dx <- deltas
                    dy <- deltas
                    guard $ not (dx == 0 && dy == 0)
                    pure { dx, dy }

tick :: (Cell -> Array Cell) -> (Int -> Boolean) -> (Int -> Boolean) -> Array Cell -> Array Cell
tick neighborsFn survivePred reproducePred livingCells = union survivors offspring
  where
    survivors = filter (livingNeighborsCount >>> survivePred) livingCells
    livingNeighborsCount c = length (intersect (neighbors c) livingCells)
    offspring = filter (livingNeighborsCount >>> reproducePred) neighboringDeadCells
    neighboringDeadCells = difference (nub (livingCells >>= neighbors)) livingCells

tick' :: Array Cell -> Array Cell
tick' = tick neighbors (\n -> n == 2 || n == 3) (_ == 3)
