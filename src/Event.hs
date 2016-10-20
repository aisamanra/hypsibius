module Event where

import           Brick (EventM, Next)
import qualified Brick
import qualified Graphics.Vty.Input.Events as Vty

import qualified State

data Event = VtyEvent Vty.Event

handle :: State.State -> Event -> EventM Int (Next State.State)
handle s (VtyEvent (Vty.EvKey Vty.KEsc _)) = Brick.halt s
handle s _ = Brick.continue s

initialize :: State.State -> EventM Int State.State
initialize s = return s
