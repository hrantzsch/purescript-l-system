module Main where

import Prelude

import Control.Apply (lift2)
import Data.Int (toNumber)
import Effect (Effect)
import Flare (UI, intRange, numberSlider)
import Flare.Drawing (Drawing, runFlareDrawing, scale, translate)

import LSystem (class LSystem, draw, grow)
import FractalTree (axiom) as FT
import SierpinskiTriangle (axiom) as ST
import FractalPlant (axiom) as FP

drawing :: forall a. LSystem a => Array a -> Int -> Drawing
drawing axiom iterations =
    let sc = 0.6 - 0.08 * toNumber iterations
        linewidth = 1.0 / sc
    in
    translate 400.0 800.0 <<< scale sc sc $
        draw (1.0 / sc) $ grow iterations axiom

createUI :: UI Drawing
createUI = drawing FP.axiom <$> intRange "iterations " 1 6 3

main :: Effect Unit
main = runFlareDrawing "controls" "canvas" createUI
