module Main where

import Prelude (Unit, bind, ($), (<>))

import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (render, scale, translate)
import Partial.Unsafe (unsafePartial)

import LSystem (draw, grow)
import FractalTree (axiom) as FT
import SierpinskiTriangle (axiom) as ST
import FractalPlant (axiom) as FP

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas

    let left = translate 150.0 700.0
    let center = translate 300.0 700.0
    let right = translate 500.0 700.0

    render ctx $
      (left $ draw $ grow 3 FT.axiom) <>
      (center $ draw $ grow 2 FT.axiom) <>
      (translate 370.0 670.0 $ scale 0.1 0.1 $ draw $ grow 7 FT.axiom) <>
      (right $ draw $ grow 4 FT.axiom) <>
      (translate 100.0 770.0 $ scale 0.3 0.3 $ draw $ grow 6 $ ST.axiom) <>
      (translate 400.0 900.0 $ scale 0.15 0.15 $  draw $ grow 6 FP.axiom)
