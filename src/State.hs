module State where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)

data Instrument = Instrument
  { instrSource :: Oscillator }
  deriving (Eq, Show)

newtype InstrRef = InstrRef { fromInstrRef :: Int }
  deriving (Eq, Show)

data Oscillator
  = OscSine
  | OscSquare
    deriving (Eq, Show)

data Note = Note
  { noteCents      :: Double
  , noteAppearance :: Text
  } deriving (Eq, Show)

newtype NoteRef = NoteRef { fromNoteRef :: Int }
  deriving (Eq, Show)

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
