module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (render, scale, translate)
import Partial.Unsafe (unsafePartial)

import LSystem (draw, grow) as LS
import FractalTree (axiom) as FT

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas

    let left = translate 150.0 700.0
    let center = translate 300.0 700.0
    let right = translate 500.0 700.0

    render ctx $
      (left $ LS.draw $ LS.grow 3 FT.axiom) <>
      (center $ LS.draw $ LS.grow 2 FT.axiom) <>
      (translate 370.0 670.0 $ scale 0.1 0.1 $ LS.draw $ LS.grow 7 FT.axiom) <>
      (right $ LS.draw $ LS.grow 4 FT.axiom)
