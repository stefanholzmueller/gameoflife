module Life where

import Prelude
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Random (Seed, random')

newtype Coords = Coords { x :: Int, y :: Int }
data CellState = Alive | Dead Int | Zombie
data Cell = Cell Coords CellState
type Population = Array Cell
type CellContext = Int  -- number of alive neighbors
type StateChangeConfig = CellState -> CellContext -> Seed -> Tuple Seed CellState

derive instance eqCoords :: Eq Coords

instance showCoords :: Show Coords
  where show (Coords { x, y }) = "(" <> show x <> ", " <> show y <> ")"

instance eqCell :: Eq Cell
  where eq (Cell c1 s1) (Cell c2 s2) = c1 == c2

instance showCell :: Show Cell
  where show (Cell coords state) = (show state) <> show coords

instance showCellState :: Show CellState
  where show Alive        = "Alive"
        show (Dead since) = "Dead" <> show since
        show Zombie       = "ZOMBIE"

isAlive :: Cell -> Boolean
isAlive (Cell _ Alive) = true
isAlive (Cell _ _)     = false

getCoords :: Cell -> Coords
getCoords (Cell coords _) = coords

gameOfLife :: Population -> Seed -> Tuple Seed Population
gameOfLife = nextGen neighbors stateChange

nextGen :: (Coords -> Array Coords) -> StateChangeConfig -> Population -> Seed -> Tuple Seed Population
nextGen neighborsConfig stateChangeConfig population seed = do updatedPopulation <- traverse nextState population
                                                               neighboringCells <- traverse nextStateInNeighboringCoords neighboringCoords
                                                               let newAliveCells = filter isAlive neighboringCells
                                                               pure (updatedPopulation <> newAliveCells)
  where
    coordsOfAliveCells = map getCoords (filter isAlive population)
    numberOfAliveNeighbors coords = length (intersect coordsOfAliveCells (neighbors coords))

    nextState (Cell coords state) = do newState <- stateChangeConfig state (numberOfAliveNeighbors coords)
                                       pure (Cell coords newState)

    populationCoords = map getCoords population
    neighboringCoords = difference (nub (populationCoords >>= neighbors)) populationCoords

    nextStateInNeighboringCoords coords = nextState (Cell coords (Dead 0))

neighbors :: Coords -> Array Coords
neighbors (Coords { x, y }) = map (\d -> Coords { x: x + d.dx, y: y + d.dy }) directions
  where
    deltas = [ -1, 0, 1 ]
    directions = do dx <- deltas
                    dy <- deltas
                    guard (dx /= 0 || dy /= 0)
                    pure { dx, dy }

stateChange :: StateChangeConfig
stateChange Alive n s | n == 2 || n == 3  = Tuple s Alive
                      | otherwise         = Tuple s (Dead 0)
stateChange (Dead since) n s | n == 3     = Tuple s Alive
                             | since == 1 = let r = random' s in Tuple r (if r < 0.3 then Zombie else Dead (since+1))
                             | otherwise  = Tuple s (Dead (since+1))
stateChange Zombie n s | n > 3            = Tuple s (Dead 0)
                       | otherwise        = Tuple s Zombie
