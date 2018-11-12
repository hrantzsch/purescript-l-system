module Fractal where

import Prelude

import Data.Array as A
import Graphics.Drawing as D
import Math (Radians, cos, pi, sin) as M

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

leaf :: D.Point
leaf = { x: 0.0, y: 20.0 }
line :: D.Point
line = { x: 0.0, y: 40.0 }
angle :: M.Radians
angle = M.pi * 0.25  -- 45 deg

rot :: D.Point -> M.Radians -> D.Point
rot vector rad = { x: vector.x * M.cos rad - vector.y * M.sin rad
                 , y: vector.x * M.sin rad - vector.y * M.cos rad }

advance :: Turtle -> Letter -> Turtle
advance t O = t { position = t.position + (rot leaf t.rotation) }
advance t I = t { position = t.position + (rot line t.rotation) }
advance t Push = t { rotation = t.rotation + angle }
advance t Pop = t

toTurtle :: Word -> Array Turtle
toTurtle = A.scanl advance newTurtle

type Turtle = { position :: D.Point, rotation :: Number }
newTurtle :: Turtle
newTurtle = { position: {x: 0.0, y: 0.0}, rotation: 0.0 }

toShape :: Array Turtle -> D.Shape
toShape = D.path <<< map \t -> t.position

draw :: Word -> D.Drawing
draw w = D.outlined (D.lineWidth 5.0) $ toShape $ toTurtle w
