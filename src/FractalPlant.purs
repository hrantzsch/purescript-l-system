module FractalPlant where

import Prelude (class Show, flip, negate, ($), (*), (/))

import Math (Radians, pi) as M

import LSystem
import Turtle (Position, move, onHead, pop, push, rotate)

{-
    variables : X F
    constants : + − [ ]
    start : X
    rules : (X → F+[[X]-X]-F[-FX]+X), (F → FF)
    angle : 25°

    * F: draw forward
    * −: turn left 25°
    * +: turn right 25°
    * X: no drawing action
    * [: push position and angle
    * ]: pop position and angle
-}

data FractalPlant = X | F | Left | Right | Push | Pop

angle :: M.Radians
angle = M.pi * 5.0 / 36.0  -- 25 deg

instance produceVariable :: LSystem FractalPlant where
    produce X = [ F, Right
                , Push, Push, X, Pop, Left, X, Pop
                , Left, F
                , Push, Left, F, X, Pop
                , Right, X]
    produce F = [F, F]
    produce Left = [Left]
    produce Right = [Right]
    produce Push = [Push]
    produce Pop = [Pop]
    advance t F = onHead t $ flip move line
    advance t X = t
    advance t Left = onHead t $ flip rotate angle
    advance t Right = onHead t $ flip rotate (negate angle)
    advance t Push = push t
    advance t Pop = pop t

instance showVariable :: Show FractalPlant where
    show X = "X"
    show F = "F"
    show Left = "Left"
    show Right = "Right"
    show Push = "Push"
    show Pop = "Pop"

axiom :: Array FractalPlant
axiom = [X]

line :: Position
line = { x: 0.0, y: 40.0 }
