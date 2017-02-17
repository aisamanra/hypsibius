{-# LANGUAGE ViewPatterns #-}

module Hypsibius.Formats.Scale (parse) where

import           Data.SCargot
import           Data.SCargot.Repr.Basic
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)
-- import qualified Data.Text as T

import           Hypsibius.Data (Note(..), Scale(..))

data Atom
  = AIdent  Text
  | AString Text
  | AInt    Integer
  | AFloat  Double
  | AKWord Text
    deriving (Eq, Show)

parseScale :: Text -> Either String Scale
parseScale = undefined

parse = undefined

{-

parse :: Text -> Either String (Seq Note)
parse t = case T.lines t of
  ((T.takeWhile (/= '#') -> "hypsibius scale"):rs) -> parseLines rs
  _ -> Left "Not a valid Hypsibius scale: missing header\n"

parseLines :: [Text] -> Either String (Seq Note)
parseLines [] = pure S.empty
parseLines (l:ls) =
  case T.words (T.takeWhile (/= '#') l) of
    [] -> parseLines ls
    [cents, name] ->
      let n = Note (read (T.unpack cents)) name
      in (n S.<|) <$> parseLines ls
    rs -> Left ("Bad declaration: " ++ show rs)
-}
