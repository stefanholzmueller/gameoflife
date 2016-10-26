module Life where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomRange, RANDOM)
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub)
import Data.Traversable (traverse)

newtype Coords = Coords { x :: Int, y :: Int }
data CellState = Alive | Dead Int | Zombie
data Cell = Cell Coords CellState
type Population = Array Cell
type CellContext = Int  -- number of alive neighbors
type StateChangeConfig eff = CellState -> CellContext -> Eff (random :: RANDOM | eff) CellState

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

--instance showPopulation :: Show Population
--  where show population =
--   where
--     coordsOfAliveCells = map getCoords (filter isAlive population)

isAlive :: Cell -> Boolean
isAlive (Cell _ Alive) = true
isAlive (Cell _ _)     = false

getCoords :: Cell -> Coords
getCoords (Cell coords _) = coords

gameOfLife :: forall eff. Population -> Eff (random :: RANDOM | eff) Population
gameOfLife = nextGen neighbors stateChange

nextGen :: forall eff. (Coords -> Array Coords) -> StateChangeConfig eff -> Population -> Eff (random :: RANDOM | eff) Population
nextGen neighborsConfig stateChangeConfig population = do updatedPopulation <- traverse nextState population
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
                    guard $ not (dx == 0 && dy == 0)
                    pure { dx, dy }

stateChange :: forall eff. StateChangeConfig eff
stateChange Alive n | n == 2 || n == 3  = pure Alive
                    | otherwise         = pure (Dead 0)
stateChange (Dead since) n | since == 1 = map (\r -> if r < 0.3 then Zombie else Dead (since+1)) (randomRange 0.0 1.0)
                           | n == 3     = pure Alive
                           | otherwise  = pure (Dead (since+1))
stateChange Zombie n | n > 3            = pure (Dead 0)
                     | otherwise        = pure Zombie
