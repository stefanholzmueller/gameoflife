module Life where

import Prelude
import Control.Monad.Cont (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomRange, RANDOM)
import Control.Monad.Writer (tell, WriterT)
import Control.MonadZero (guard)
import Data.Array (difference, filter, intersect, length, nub)
import Data.Traversable (traverse)

newtype Coords = Coords { x :: Int, y :: Int }
data CellState = Alive | Dead Int | Zombie
data Cell = Cell Coords CellState
type Population = Array Cell
type CellContext = Int  -- number of alive neighbors
type StateChangeConfig = CellState -> CellContext -> LifeMonad CellState
type LifeMonad a = forall eff. WriterT String (Eff (random :: RANDOM | eff)) a

derive instance eqCoords :: Eq Coords
derive instance eqCellState :: Eq CellState

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

gameOfLife :: Population -> LifeMonad Population
gameOfLife = nextGen neighbors stateChange

nextGen :: (Coords -> Array Coords) -> StateChangeConfig -> Population -> LifeMonad Population
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
                    guard (dx /= 0 || dy /= 0)
                    pure { dx, dy }

stateChange :: StateChangeConfig
stateChange Alive n | n == 2 || n == 3  = pure Alive
                    | otherwise         = pure (Dead 0)
stateChange (Dead since) n | n == 3     = pure Alive
                           | since == 1 = do r <- lift (randomRange 0.0 1.0)
                                             let cs = if r < 0.3 then Zombie else Dead (since+1)
                                             tell (if cs == Zombie then "Z" else "")
                                             pure cs
                           | otherwise  = pure (Dead (since+1))
stateChange Zombie n | n > 3            = pure (Dead 0)
                     | otherwise        = pure Zombie
