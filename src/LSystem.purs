module LSystem where

import Prelude (map, ($), (-), (<<<))

import Data.Array (concatMap, fold)
import Graphics.Drawing as D

import Turtle

class LSystem a where
    -- | Apply the system's rules to replace a variable
    produce :: a -> Array a
    -- | Advance the `Turtle` according to the instructions for each Variable.
    advance :: TurtleStack -> a -> TurtleStack

grow :: forall a. LSystem a => Int -> Array a -> Array a
grow 0 w = w
grow n w = concatMap produce $ grow (n-1) w

render :: forall a. LSystem a => Array a -> Array Turtle
render w = walk w advance

drawPath :: TurtlePath -> D.Drawing
drawPath = D.outlined (D.lineWidth 4.0) <<< D.path <<< map (_.position)

draw :: forall a. LSystem a => Array a -> D.Drawing
draw = fold <<< map drawPath <<< spans <<< render
