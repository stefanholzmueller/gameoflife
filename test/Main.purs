module Test.Main where

import Prelude
import Life as L
import Data.Array (length)
import Data.Foldable (all, elem)
import Test.QuickCheck (quickCheck, QC)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)

newtype RandomCell = RandomCell L.Cell
instance arbitraryCell :: Arbitrary RandomCell where
  arbitrary = do
              x <- chooseInt 0 99
              y <- chooseInt 0 99
              pure $ RandomCell $ Cell { x, y }

main :: forall eff. QC eff Unit
main = do
    quickCheck everyCellHasEightNeighbors
    quickCheck aCellsNeighborHasNeighborsContainingTheCell
  where
    everyCellHasEightNeighbors (RandomCell cell) =
        length (L.neighbors cell) == 8
    aCellsNeighborHasNeighborsContainingTheCell (RandomCell cell) =
        all (_ == true) do
          n <- L.neighbors cell
          let ns = L.neighbors n
          pure (elem cell ns)
