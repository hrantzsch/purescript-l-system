module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (render, translate)
import Partial.Unsafe (unsafePartial)

import Fractal (Letter(..), draw, grow)

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas

    render ctx $
      translate 300.0 300.0 $
        draw $ grow $ grow $ [O]
