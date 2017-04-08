module Hypsibius.Formats where

import           Data.Adnot
import           Data.Sequence (Seq)
import qualified Data.ByteString as BS

import           Hypsibius.Data

readScale :: FilePath -> IO (Either String Scale)
readScale = fmap decode . BS.readFile
