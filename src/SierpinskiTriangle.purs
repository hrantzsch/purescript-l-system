module SierpinskiTriangle where

import Prelude (class Show, flip, negate, ($), (*), (/))
import Math (Radians, pi) as M

import LSystem
import Turtle (Position, move, onHead, rotate)

{-
    variables : F G
    constants : + −
    start : F−G−G
    rules : (F → F−G+F+G−F), (G → GG)
    angle : 120°

    * F and G both mean "draw forward"
    * + means "turn left by angle"
    * − means "turn right by angle"
-}

data SierpinskiTriangle = F | G | Left | Right

angle :: M.Radians
angle = M.pi * 2.0 / 3.0  -- 120 deg

instance produceVariable :: LSystem SierpinskiTriangle where
    produce F = [F, Right, G, Left, F, Left, G, Right, F]
    produce G = [G, G]
    produce Left = [Left]
    produce Right = [Right]
    advance t F = onHead t $ flip move line
    advance t G = onHead t $ flip move line
    advance t Left = onHead t $ flip rotate angle
    advance t Right = onHead t $ flip rotate (negate angle)

instance showVariable :: Show SierpinskiTriangle where
    show F = "F"
    show G = "G"
    show Left = "Left"
    show Right = "Right"

axiom :: Array SierpinskiTriangle
axiom = [F, Right, G, Right, G]

line :: Position
line = { x: 0.0, y: 40.0 }
