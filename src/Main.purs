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

    render ctx $
      translate 400.0 800.0 $
        {-- scale 0.4 0.4 $ --}
          draw $ grow $ grow $ grow $ grow $ [O]
