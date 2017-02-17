module Hypsibius.Event where

import           Brick (BrickEvent, EventM, Next)
import qualified Brick
import qualified Graphics.Vty.Input.Events as Vty

import qualified Hypsibius.State as State

data Event = Event

handle :: State.State -> BrickEvent Int Event -> EventM Int (Next State.State)
handle s (Brick.VtyEvent (Vty.EvKey Vty.KEsc _)) = Brick.halt s
handle s _ = Brick.continue s

initialize :: State.State -> EventM Int State.State
initialize s = return s
