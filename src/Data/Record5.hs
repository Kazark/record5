module Data.Record5 where

data Delimiter
  = Comma | Pipe | Space
  deriving (Eq, Bounded, Ord, Enum)

instance Show Delimiter where
  -- This is a standard extension,
  show Comma = ".csv"
  -- but these two are not, as far as I know.
  show Pipe = ".psv"
  show Space = ".ssv"

-- | A little trick I learned from:
-- | https://stackoverflow.com/a/53645493/834176
-- | to ensure that the parser and printer stay in sync.
parseDelim :: String -> Maybe Delimiter
parseDelim s = lookup s tab where
  -- If this were a large datatype or frequently used, we might want to ensure
  -- this table was only calculated once. But it's neither large nor frequently
  -- used.
  tab = [(show x, x) | x <- [minBound..maxBound]]
