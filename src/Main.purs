module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (render, scale, translate)
import Partial.Unsafe (unsafePartial)

import Fractal (Letter(..), draw, grow)

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas

    let left = translate 150.0 700.0
    let center = translate 300.0 700.0
    let right = translate 500.0 700.0

    render ctx $
      (left $ draw $ grow 3 [O]) <>
      (center $ draw $ grow 2 [O]) <>
      (right $ draw $ grow 4 [O])
