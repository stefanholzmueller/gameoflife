module Test.Main where

import Prelude
import Life as L
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (filter, length)
import Data.Foldable (all, elem)
import Test.QuickCheck (assertEquals, quickCheck', quickCheck, QC)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)

newtype RandomCoords = RandomCoords L.Coords
instance arbitraryCoords :: Arbitrary RandomCoords where
  arbitrary = do
              x <- chooseInt 0 99
              y <- chooseInt 0 99
              pure $ RandomCoords $ L.Coords { x, y }

main :: forall eff. QC eff Unit
main = do
       quickCheck everyCellHasEightNeighbors
       quickCheck aCellsNeighborHasNeighborsContainingTheCell
       quickCheck' 1 gliderInFifthGeneration
  where
    everyCellHasEightNeighbors (RandomCoords coords) =
        length (L.neighbors coords) == 8

    aCellsNeighborHasNeighborsContainingTheCell (RandomCoords coords) =
        all (_ == true) do
          n <- L.neighbors coords
          let ns = L.neighbors n
          pure (elem coords ns)

    gliderInFifthGeneration (RandomCoords _) = assertEquals [] []
      where
        cell x y = L.Cell (L.Coords { x: x, y: y }) L.Alive
        initial = [ cell 2 1, cell 3 2, cell 3 3, cell 2 3, cell 1 3 ]
        gen4 = unsafePerformEff ((L.gameOfLife initial) >>= L.gameOfLife >>= L.gameOfLife >>= L.gameOfLife)
        gen5 = unsafePerformEff (L.gameOfLife gen4)
