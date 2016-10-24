{-# LANGUAGE TemplateHaskell #-}

module Hypsibius.Data where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)
import           Data.Word (Word8)
import           Lens.Family2.TH

data Oscillator
  = OscSine
  | OscSquare
    deriving (Eq, Show)

data Instrument = Instrument
  { _instrSource :: Oscillator
  } deriving (Eq, Show)

$(makeLenses ''Instrument)

newtype InstrRef = InstrRef { _fromInstrRef :: Int }
  deriving (Eq, Show)

$(makeLenses ''InstrRef)

data Note = Note
  { _noteCents      :: Double
  , _noteAppearance :: Text
  } deriving (Eq, Show)

$(makeLenses ''Note)

newtype NoteRef = NoteRef { _fromNoteRef :: Int }
  deriving (Eq, Show)

$(makeLenses ''NoteRef)

data Scale = Scale
  { _scaleName       :: Text
  , _scaleTotalCents :: Double
  , _scaleNotes      :: Seq Note
  } deriving (Eq, Show)

$(makeLenses ''Scale)

data Event = Event
  deriving (Eq, Show)

data Track = Track
  {
  } deriving (Eq, Show)

data Beats
  = BeatsSimple Word8
  | BeatsAdditive [Word8]
  | BeatsFractional Word8 Word8
    deriving (Eq, Show)

$(makeTraversals ''Beats)

data Signature = Signature
  { _sigPerBar   :: Beats
  , _sigBeatUnit :: Word8
  } deriving (Eq, Show)

$(makeLenses ''Signature)

data Song = Song
  { _songScale  :: Scale
  , _songTracks :: Seq Track
  } deriving (Eq, Show)

$(makeLenses ''Song)
