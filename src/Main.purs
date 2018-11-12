module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (render)
import Partial.Unsafe (unsafePartial)

import Fractal (Letter(..), draw, grow)

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas

    render ctx $
      draw $ grow $ grow $ [O]
