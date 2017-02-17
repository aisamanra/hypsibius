{-# LANGUAGE TemplateHaskell #-}

module Hypsibius.Data where

import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Data.Word (Word8)
import           Lens.Family2.TH

-- | XXX: This is a temporary definition of 'Oscillator' for early
--   prototyping purposes.
data Oscillator
  = OscSine
  | OscSquare
    deriving (Eq, Show)

-- | XXX: This is a temporary definition of 'Instrument' for early
--   prototyping purposes.
data Instrument = Instrument
  { _instrSource :: Oscillator
  } deriving (Eq, Show)

$(makeLenses ''Instrument)


-- | We'll maintain a list of instruments and refer to them using
--   indices. For type safety, here is a wrapper around those
--   indices.
newtype InstrRef = InstrRef { _fromInstrRef :: Int }
  deriving (Eq, Show)

$(makeLenses ''InstrRef)

-- | A 'Note' here is an individual element of a scale, which we'll
--   maintain a unique list of on a per-song basis, and most of the time
--   we'll use indices into that list. A 'Note' has a frequency represented
--   in cents and an appearance that the user will see when running the
--   program, which should be no more than a few characters long.
data Note = Note
  { _noteCents      :: Double
  , _noteAppearance :: Text
  } deriving (Eq, Show)

$(makeLenses ''Note)

-- | We'll maintain a list of notes and refer to them using indices. For type
--   safety, here is a wrapper around those indices.
newtype NoteRef = NoteRef { _fromNoteRef :: Int }
  deriving (Eq, Show)

$(makeLenses ''NoteRef)

-- | A 'Scale' has a name, a total number of cents (which will almost always be
--   1200 for traditional scales) and a list of notes associated with it.
data Scale = Scale
  { _scaleName       :: Text
  , _scaleTotalCents :: Double
  , _scaleNotes      :: Seq Note
  } deriving (Eq, Show)

$(makeLenses ''Scale)

-- | An 'Event' is a typical event associated with a song.
data Event = Event
  deriving (Eq, Show)

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


data TrackChunk = TrackChunk
  { _tcSignature :: Signature
  } deriving (Eq, Show)


data Track = Track
  {
  } deriving (Eq, Show)



data Song = Song
  { _songScale  :: Scale
  , _songTracks :: Seq Track
  } deriving (Eq, Show)

$(makeLenses ''Song)
