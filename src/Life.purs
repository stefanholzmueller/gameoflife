module Life where

import Prelude
import Control.Monad.State (get, put, State)
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub)
import Data.Traversable (traverse)
import Random (Seed, random')

newtype Coords = Coords { x :: Int, y :: Int }
data CellState = Alive | Dead Int | Zombie
data Cell = Cell Coords CellState
type Population = Array Cell
type CellContext = Int  -- number of alive neighbors
type NeighborsConfig = Coords -> Array Coords
type StateChangeConfig = CellState -> CellContext -> State Seed CellState

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

gameOfLife :: Population -> State Seed Population
gameOfLife = nextGen neighbors stateChange

nextGen :: NeighborsConfig -> StateChangeConfig -> Population -> State Seed Population
nextGen neighborsConfig stateChangeConfig population = do updatedPopulation <- traverse nextState population
                                                          neighboringCells <- traverse nextStateInNeighboringCoords neighboringCoords
                                                          let newAliveCells = filter isAlive neighboringCells
                                                          pure (updatedPopulation <> newAliveCells)

  where
    coordsOfAliveCells = map getCoords (filter isAlive population)
    numberOfAliveNeighbors coords = length (intersect coordsOfAliveCells (neighbors coords))

    nextState :: Cell -> State Seed Cell
    nextState (Cell coords state) = map (\newState -> Cell coords newState) (stateChangeConfig state (numberOfAliveNeighbors coords))

    populationCoords = map getCoords population
    neighboringCoords = difference (nub (populationCoords >>= neighbors)) populationCoords

    nextStateInNeighboringCoords :: Coords -> State Seed Cell
    nextStateInNeighboringCoords coords = nextState (Cell coords (Dead 0))

neighbors :: NeighborsConfig
neighbors (Coords { x, y }) = map (\d -> Coords { x: x + d.dx, y: y + d.dy }) directions
  where
    deltas = [ -1, 0, 1 ]
    directions = do dx <- deltas
                    dy <- deltas
                    guard (dx /= 0 || dy /= 0)
                    pure { dx, dy }

stateChange :: StateChangeConfig
stateChange Alive n | n == 2 || n == 3  = pure Alive
                    | otherwise         = pure (Dead 0)
stateChange (Dead since) n | n == 3     = pure Alive
                           | since == 1 = do s <- get
                                             let r = random' s
                                             put r
                                             pure (if r < 0.3 then Zombie else Dead (since+1))
                           | otherwise  = pure (Dead (since+1))
stateChange Zombie n | n > 3            = pure (Dead 0)
                     | otherwise        = pure Zombie
