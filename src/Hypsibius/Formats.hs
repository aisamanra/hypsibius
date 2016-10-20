module Hypsibius.Formats where

import           Data.Sequence (Seq)
import qualified Data.Text.IO as T

import qualified Hypsibius.Formats.Scale as Scale
import           Hypsibius.Data (Note)

readScale :: FilePath -> IO (Either String (Seq Note))
readScale = fmap Scale.parse . T.readFile
