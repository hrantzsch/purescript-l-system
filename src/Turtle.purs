module Turtle where

import Prelude

import Data.Array (scanl, span, uncons, (:))
import Data.Array.NonEmpty (NonEmptyArray, cons', fromArray, head, singleton, tail)
import Data.Maybe (Maybe(..))
import Math (Radians, cos, sin) as M

-- | Cursor for drawing Logo-like turtle graphics.
type Turtle = { position :: Position
              , rotation :: Number
              , drawing :: Boolean }
-- | A stack of `Turtle`, used for push and pop operations.
type TurtleStack = NonEmptyArray Turtle
-- | The path of a `Turtle` to draw.
type TurtlePath = Array Turtle

type Position = { x :: Number, y :: Number }

newTurtle :: Turtle
newTurtle = { position: {x: 0.0, y: 0.0}
            , rotation: 0.0
            , drawing: true }

-- | Rotate a vector.
rot :: Position -> M.Radians -> Position
rot vector rad = { x: vector.x * M.cos rad - vector.y * M.sin rad
                 , y: vector.x * M.sin rad - vector.y * M.cos rad }

-- | Move the `Turtle` based on it's position and rotation.
move :: Turtle -> Position -> Turtle
move t v = t { position = t.position + (rot v t.rotation), drawing = true }

-- | Rotate the `Turtle`.
rotate :: Turtle -> M.Radians -> Turtle
rotate t a = t { rotation = t.rotation + a, drawing = true }

-- | Apply a function to the head of a `TurtleStack`.
onHead :: TurtleStack -> (Turtle -> Turtle) -> TurtleStack
onHead t f = f (head t) `cons'` tail t

-- | Remove the head of a `TurtleStack`.
-- | If the stack is emtpy afterwards, return `fallback` as a singleton.
safeTail :: TurtleStack -> Turtle -> TurtleStack
safeTail t fallback = case (fromArray $ tail t) of
      Just some -> some
      Nothing   -> singleton fallback

spans :: TurtlePath -> Array (TurtlePath)
spans [] = [[]]
spans t = split.init : spans rest
    where split = span (_.drawing) t
          rest = case uncons split.rest of
                      Just { head: x, tail: xs } -> x { drawing = true } : xs
                      Nothing -> []

walk :: forall a. Array a -> (TurtleStack -> a -> TurtleStack) -> TurtlePath
walk as f = map head $ scanl f (singleton newTurtle) as
