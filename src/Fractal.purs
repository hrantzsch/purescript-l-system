module Fractal where

import Prelude ((-))

import Data.Array as A
import Graphics.Drawing (Drawing, circle, lineWidth, outlined)
import Data.Show

{-
    variables : 0, 1
    constants: [, ]
    axiom : 0
    rules : (1 → 11), (0 → 1[0]0)

    0: draw a line segment ending in a leaf
    1: draw a line segment
    [: push position and angle, turn left 45 degrees
    ]: pop position and angle, turn right 45 degrees
-}

data Letter = O | I | Push | Pop
type Word = Array Letter

instance showLetter :: Show Letter where
    show O = "0"
    show I = "1"
    show Push = "["
    show Pop = "]"

produce :: Letter -> Array Letter
produce O = [I, Push, O, Pop, O]
produce I = [I, I]
produce Push = [Push]
produce Pop = [Pop]

grow :: Word -> Word
grow = A.concatMap produce

draw :: Word -> Drawing
draw w = outlined (lineWidth 4.0) (circle (0.0-100.0) 30.0 200.0)
