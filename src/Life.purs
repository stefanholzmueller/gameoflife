module Life where

import Prelude
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomRange, RANDOM)
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub)
import Data.Traversable (sequence, traverse)

newtype Coords = Coords { x :: Int, y :: Int }
data CellState = Alive | Dead Int | Zombie
data Cell = Cell Coords CellState
type CellContext = Int  -- number of alive neighbors
type Population = Array Cell
type StateChangeConfig = CellState -> CellContext -> Eff (random :: RANDOM) CellState

instance eqCoords :: Eq Coords
  where eq (Coords c1) (Coords c2) = c1.x == c2.x && c1.y == c2.y

isAlive :: Cell -> Boolean
isAlive (Cell _ Alive) = true
isAlive (Cell _ _)     = false

getCoords :: Cell -> Coords
getCoords (Cell coords _) = coords

gameOfLife :: Population -> Eff (random :: RANDOM) Population
gameOfLife = nextGen neighbors stateChange

nextGen :: (Coords -> Array Coords) -> StateChangeConfig -> Population -> Eff (random :: RANDOM) Population
nextGen neighborsConfig stateChangeConfig population = lift2 append (traverse nextState population) offspring
  where
    coordsOfAliveCells = (map getCoords (filter isAlive population)) :: Array Coords
    numberOfAliveNeighbors coords = (length (intersect coordsOfAliveCells (neighbors coords))) :: Int
    nextState (Cell coords state) = (map (\s -> Cell coords s) (stateChangeConfig state (numberOfAliveNeighbors coords))) :: Eff (random :: RANDOM) Cell
    populationCoords = (map getCoords population) :: Array Coords
    neighboringCoords = (difference (nub (populationCoords >>= neighbors)) populationCoords) :: Array Coords
    nextStateInNeighboringCoords coords = (map (\s -> Cell coords s) (stateChangeConfig (Dead 0) (numberOfAliveNeighbors coords))) :: Eff (random :: RANDOM) Cell
    neighboringCells = (sequence (map nextStateInNeighboringCoords neighboringCoords)) :: Eff (random :: RANDOM) (Array Cell)
    offspring = (map (\ss -> filter isAlive ss) neighboringCells) :: Eff (random :: RANDOM) (Array Cell)

neighbors :: Coords -> Array Coords
neighbors (Coords { x, y }) = map (\d -> Coords { x: d.dx + x, y: d.dy + y }) directions
  where
    deltas = [ -1, 0, 1 ]
    directions = do dx <- deltas
                    dy <- deltas
                    guard $ not (dx == 0 && dy == 0)
                    pure { dx, dy }

stateChange :: StateChangeConfig
stateChange Alive n | n == 2 || n == 3  = pure Alive
                    | otherwise         = pure (Dead 0)
stateChange (Dead since) n | since == 2 = map (\r -> if r < 0.3 then Zombie else Dead (since+1)) (randomRange 0.0 1.0)
                           | n == 3     = pure Alive
                           | otherwise  = pure (Dead (since+1))
stateChange Zombie n | n > 3            = pure (Dead 0)
                     | otherwise        = pure Zombie
