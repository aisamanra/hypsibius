module Hypsibius.State where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)

import           Hypsibius.Data

data State = State
  { stateFile        :: Maybe FilePath
  , stateInstruments :: Seq Instrument
  , stateScale       :: Seq Note
  } deriving (Show)

newState :: State
newState = State
  { stateFile        = Nothing
  , stateInstruments = S.empty
  , stateScale       = S.empty
  }
