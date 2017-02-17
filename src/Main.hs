module Main where

import           Brick
import qualified Brick.BChan as Brick
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
  , appAttrMap      = \ _ -> attrMap mempty []
  }

main :: IO ()
main = do
  eventChan <- Brick.newBChan 32
  _ <- customMain (Vty.mkVty mempty) (Just eventChan) trackerApp State.newState
  return ()
