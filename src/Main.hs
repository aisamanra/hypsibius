module Main where

import           Brick
import qualified Control.Concurrent.Chan as Chan
import           Data.Default (def)
import qualified Graphics.Vty as Vty


import qualified Hypsibius.State as State
import qualified Hypsibius.Draw as Draw
import qualified Hypsibius.Event as Event
import qualified Hypsibius.Formats as Formats

trackerApp :: App State.State Event.Event Int
trackerApp = App
  { appDraw         = Draw.draw
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent  = Event.handle
  , appStartEvent   = Event.initialize
  , appAttrMap      = def
  , appLiftVtyEvent = Event.VtyEvent
  }

main :: IO ()
main = do
  eventChan <- Chan.newChan
  _ <- customMain (Vty.mkVty def) eventChan trackerApp State.newState
  return ()
