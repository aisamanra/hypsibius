module Draw where

import Brick

import State

draw :: State -> [Widget Int]
draw _ = [str "whoo"]
