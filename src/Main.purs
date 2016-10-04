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

initialState :: State
initialState = { cells: [ L.cell 2 1, L.cell 3 2, L.cell 3 3, L.cell 2 3, L.cell 1 3 -- glider
                        , L.cell 11 2, L.cell 12 2, L.cell 13 2 -- blinker
                        , L.cell 2 11, L.cell 3 11, L.cell 2 12, L.cell 3 12 -- block
                        , L.cell 31 20, L.cell 32 20, L.cell 30 21, L.cell 31 21, L.cell 31 22 -- r-pentomino
                        ] }

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state = HH.div_ (map renderCell state.cells)
    where
    renderCell (L.Cell {x, y}) = HH.div [ HP.class_ $ HC.className "cell"
                                      , HS.style do left $ px $ toNumber $ 32 * x
                                                    top $ px $ toNumber $ 32 * y
                                      ] []

  eval :: Query ~> H.ComponentDSL State Query g
  eval (Tick next) = do
    H.modify (\state -> { cells: L.tick' state.cells })
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