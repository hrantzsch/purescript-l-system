module Main where

import Prelude (Unit, ($), (/), (<<<))
import Control.Apply (lift2)

import Effect (Effect)
import Flare (UI, int, numberSlider)
import Flare.Drawing (Drawing, runFlareDrawing, scale, translate)

import LSystem (class LSystem, draw, grow)
import FractalTree (axiom) as FT
import SierpinskiTriangle (axiom) as ST
import FractalPlant (axiom) as FP

drawing :: forall a. LSystem a => Array a -> Int -> Number -> Drawing
drawing axiom iterations sc =
    translate 400.0 800.0 <<< scale sc sc $
        draw (1.0 / sc) $ grow iterations axiom

draw' :: forall a. LSystem a => Array a -> Drawing
draw' axiom = drawing axiom 3 0.5

createUI :: UI Drawing
createUI = lift2 (drawing FP.axiom)
    (int "iterations" 3)
    (numberSlider "scale" 0.1 1.0 0.1 1.0)

main :: Effect Unit
main = runFlareDrawing "controls" "canvas" createUI
