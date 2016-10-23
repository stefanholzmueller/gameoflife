module Main where

import Halogen as H
import Halogen.HTML.CSS.Indexed as HS
import Halogen.HTML.Core as HC
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Life as L
import CSS.Geometry (left, top)
import CSS.Size (px)
import Control.Monad.Aff (Aff, later')
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import Halogen.Util (awaitBody, runHalogenAff)
import Prelude hiding (top)

data Query a = Tick a

type State = { cells :: Array L.Cell }

cell :: Int -> Int -> L.Cell
cell x y = L.Cell (L.Coords { x: x, y: y }) L.Alive

toString :: L.CellState -> String
toString L.Alive    = "alive"
toString (L.Dead _) = "dead"
toString L.Zombie   = "zombie"

initialState :: State
initialState = { cells: [ cell 2 1, cell 3 2, cell 3 3, cell 2 3, cell 1 3 -- glider
                        , cell 11 2, cell 12 2, cell 13 2 -- blinker
                        , cell 2 11, cell 3 11, cell 2 12, cell 3 12 -- block
                        , cell 31 20, cell 32 20, cell 30 21, cell 31 21, cell 31 22 -- r-pentomino
                        ] }

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where
    render :: State -> H.ComponentHTML Query
    render state = HH.div_ (map renderCell state.cells)
      where
        renderCell (L.Cell (L.Coords {x, y}) state) = HH.div [ HP.class_ $ HC.className $ toString state
                                                             , HS.style do left $ px $ toNumber $ 32 * x
                                                                           top $ px $ toNumber $ 32 * y
                                                             ] []

    eval :: Query ~> H.ComponentDSL State Query g
    eval (Tick next) = do
      H.modify (\state -> { cells: L.gameOfLife state.cells })
      pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  setInterval 1000 $ driver (H.action Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
