module Hypsibius.Data where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)

data Instrument = Instrument
  { _instrSource :: Oscillator
  } deriving (Eq, Show)

newtype InstrRef = InstrRef { fromInstrRef :: Int }
  deriving (Eq, Show)

data Oscillator
  = OscSine
  | OscSquare
    deriving (Eq, Show)

data Note = Note
  { _noteCents      :: Double
  , _noteAppearance :: Text
  } deriving (Eq, Show)

newtype NoteRef = NoteRef { fromNoteRef :: Int }
  deriving (Eq, Show)

data Scale = Scale
  { _scaleName       :: Text
  , _scaleTotalCents :: Double
  , _scaleNotes      :: Seq Note
  } deriving (Eq, Show)

data Event = Event
  deriving (Eq, Show)

data Track = Track
  {
  } deriving (Eq, Show)

data Song = Song
  { _songScale  :: Scale
  , _songTracks :: Seq Track
  } deriving (Eq, Show)
