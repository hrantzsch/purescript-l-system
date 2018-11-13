module Fractal where

import Prelude

import Data.Array (concatMap, scanl)
import Data.Array.NonEmpty (NonEmptyArray, cons', fromArray, head, singleton, tail, (:))
import Data.Maybe
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
grow = concatMap produce

leaf :: D.Point
leaf = { x: 0.0, y: 20.0 }
line :: D.Point
line = { x: 0.0, y: 40.0 }
angle :: M.Radians
angle = M.pi * 0.25  -- 45 deg

rot :: D.Point -> M.Radians -> D.Point
rot vector rad = { x: vector.x * M.cos rad - vector.y * M.sin rad
                 , y: vector.x * M.sin rad - vector.y * M.cos rad }

type Turtle = { position :: D.Point, rotation :: Number }
type TurtleStack = NonEmptyArray Turtle

initialStack :: TurtleStack
initialStack = singleton { position: {x: 0.0, y: 0.0}, rotation: 0.0 }

-- move the Turtle based on it's position and rotation
move :: Turtle -> D.Point -> Turtle
move t v = t { position = t.position + (rot v t.rotation) }

rotate :: Turtle -> M.Radians -> Turtle
rotate t a = t { rotation = t.rotation + a}

advance :: TurtleStack -> Letter -> TurtleStack
advance t O = cons' (move (head t) leaf) (tail t)
advance t I = cons' (move (head t) line) (tail t)
advance t Push = rotate (head t) angle : t
advance t Pop = cons' (rotate (head popped) (-angle)) (tail popped)
    where popped = case (fromArray $ tail t) of
                    Just some -> some
                    Nothing   -> initialStack

toTurtle :: Word -> Array TurtleStack
toTurtle = scanl advance $ initialStack

draw :: Word -> D.Drawing
draw = D.outlined (D.lineWidth 5.0) <<< positions <<< toTurtle
    where positions = D.path <<< map \t -> (head t).position
