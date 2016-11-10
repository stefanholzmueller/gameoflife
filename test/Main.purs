module Test.Main where

import Prelude
import Life as L
import Data.Array (length, partition)
import Data.Foldable (all, elem)
import Data.Int (toNumber)
import Data.List.Lazy (drop, iterate, take, toUnfoldable)
import Random (random')
import Test.QuickCheck (quickCheck', quickCheck, QC)
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
       quickCheck randomCycledHasCorrectRange
       quickCheck randomCycledHasCorrectRange
       quickCheck' 1 randomIsUniformlyDistributed
  where
    everyCellHasEightNeighbors (RandomCoords coords) =
        length (L.neighbors coords) == 8

    aCellsNeighborHasNeighborsContainingTheCell (RandomCoords coords) =
        all (_ == true) do
          n <- L.neighbors coords
          let ns = L.neighbors n
          pure (elem coords ns)

    randomHasCorrectRange (initial :: Number) =
        let r = random' initial
        in r >= 0.0 && r < 1.0

    randomCycledHasCorrectRange (initial :: Number) =
        let r = random' (random' initial)
        in r >= 0.0 && r < 1.0

    randomIsUniformlyDistributed (initial :: Number) =
        let rs = take 500 (drop 1 (iterate random' initial))
            p  = partition (\r -> r < 0.5) (toUnfoldable rs)
            quota = toNumber (length p.yes) / (toNumber (length p.no))
            ratio = if quota < 1.0 then 1.0 / quota else quota
        in ratio <= 1.1
