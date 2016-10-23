module Life where

import Prelude
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub)

newtype Coords = Coords { x :: Int, y :: Int }
data CellState = Alive | Dead Int | Zombie
data Cell = Cell Coords CellState
type CellContext = Int  -- number of alive neighbors
type Population = Array Cell

instance eqCoords :: Eq Coords
  where eq (Coords c1) (Coords c2) = c1.x == c2.x && c1.y == c2.y

isAlive :: Cell -> Boolean
isAlive (Cell _ Alive) = true
isAlive (Cell _ _)     = false

getCoords :: Cell -> Coords
getCoords (Cell coords _) = coords

gameOfLife :: Population -> Population
gameOfLife = nextGen neighbors stateChange

nextGen :: (Coords -> Array Coords) -> (CellState -> CellContext -> CellState) -> Population -> Population
nextGen neighborsConfig stateChangeConfig population = (map nextState population) <> offspring
  where
    coordsOfAliveCells = map getCoords (filter isAlive population)
    numberOfAliveNeighbors coords = length (intersect coordsOfAliveCells (neighbors coords))
    nextState (Cell coords state) = Cell coords (stateChangeConfig state (numberOfAliveNeighbors coords))
    populationCoords = map getCoords population
    neighboringCoords = difference (nub (populationCoords >>= neighbors)) populationCoords
    nextStateInNeighboringCoords coords = Cell coords (stateChangeConfig (Dead 0) (numberOfAliveNeighbors coords))
    offspring = filter isAlive (map nextStateInNeighboringCoords neighboringCoords)

neighbors :: Coords -> Array Coords
neighbors (Coords { x, y }) = map (\d -> Coords { x: d.dx + x, y: d.dy + y }) directions
  where
    deltas = [ -1, 0, 1 ]
    directions = do dx <- deltas
                    dy <- deltas
                    guard $ not (dx == 0 && dy == 0)
                    pure { dx, dy }

stateChange :: CellState -> CellContext -> CellState
stateChange Alive n | n == 2 || n == 3  = Alive
                    | otherwise         = Dead 0
stateChange (Dead since) n | since == 2 = Zombie -- TODO 30% chance
                           | n == 3     = Alive
                           | otherwise  = Dead (since+1)
stateChange Zombie n | n > 3            = Dead 0
                     | otherwise        = Zombie
