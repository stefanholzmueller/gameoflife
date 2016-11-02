module Random where

import Prelude
import Math (floor, sin)

type Seed = Number

random' :: Seed -> Number
random' seed = let x = (sin seed) * 1000000.0
               in (x - floor x)
