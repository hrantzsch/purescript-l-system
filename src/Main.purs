module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, render, scale, translate)
import Partial.Unsafe (unsafePartial)

import LSystem (class LSystem)
import LSystem (draw, grow) as LS
import FractalPlant (axiom) as FP

drawOnCanvas :: Drawing -> Effect Unit
drawOnCanvas drawing = do
    mcanvas <- getCanvasElementById "canvas"
    ctx <- getContext2D $ unsafePartial $ fromJust mcanvas
    render ctx drawing

scaleAndDraw :: forall a. LSystem a => Array a -> Drawing
scaleAndDraw = translate 400.0 800.0 <<< scale 0.4 0.4 <<< LS.draw (2.0)

main :: Effect Unit
main = drawOnCanvas $ scaleAndDraw $ LS.grow 4 FP.axiom
