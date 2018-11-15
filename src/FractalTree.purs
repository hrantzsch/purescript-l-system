module FractalTree where

import Prelude (class Show, flip, negate, ($), (*), (<<<))

import Data.Array.NonEmpty (cons, head)
import Math (Radians, pi) as M

import LSystem
import Turtle (Position, move, newTurtle, onHead, rotate, safeTail)

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

data FractalTree = O | I | Push | Pop

instance produceVariable :: LSystem FractalTree where
    produce O = [I, Push, O, Pop, O]
    produce I = [I, I]
    produce Push = [Push]
    produce Pop = [Pop]
    advance t O = onHead t $ flip move leaf
    advance t I = onHead t $ flip move line
    advance t Push = cons (rotate (head t) angle) t
    advance t Pop = onHead (safeTail t newTurtle) $
                      (\h -> h { drawing = false }) <<< flip rotate (-angle)

instance showVariable :: Show FractalTree where
    show O = "0"
    show I = "1"
    show Push = "["
    show Pop = "]"

axiom :: Array FractalTree
axiom = [O]

leaf :: Position
leaf = { x: 0.0, y: 20.0 }
line :: Position
line = { x: 0.0, y: 40.0 }
angle :: M.Radians
angle = M.pi * 0.25  -- 45 deg
