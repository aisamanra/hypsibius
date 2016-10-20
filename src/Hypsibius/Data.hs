module Hypsibius.Data where

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

data Scale = Scale
  { scaleName       :: Text
  , scaleTotalCents :: Double
  , scaleNotes      :: Seq Note
  } deriving (Eq, Show)
