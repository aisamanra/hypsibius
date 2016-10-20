module Main where

import           Brick
import qualified Control.Concurrent.Chan as Chan
import           Data.Default (def)
import qualified Graphics.Vty as Vty


import qualified State
import qualified Draw
import qualified Event

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