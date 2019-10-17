{-# LANGUAGE TemplateHaskell #-}
module Data.Record5 where

import           Data.Colour (Colour)
import           Data.Time (Day)

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
-- | We could use invertible syntax descriptions here, but this demonstrates the
-- | fact that there are lighter-weight ways of keeping your parser and printer
-- | in sync for certain specialized cases. Also, do not get confused! There are
-- | _two_ syntaxes relevant to delimiters! One is the syntax of the file
-- | extension; the other, internal to the contents of the file. It will help
-- | avoid confusion if we don't use invertible syntax descriptions for what is
-- | external to the file's contents.
parseDelim :: String -> Maybe Delimiter
parseDelim s = lookup s tab where
  -- If this were a large datatype or frequently used, we might want to ensure
  -- this table was only calculated once. But it's neither large nor frequently
  -- used.
  tab = [(show x, x) | x <- [minBound..maxBound]]


data Gender = Male | Female

-- A shockingly rich color type
type Color = Colour Double -- haha, thanks Brits, very convenient

data Record5
            -- Technically, String is too big of a type for a name, if we wanted
            -- to be precise. Considering the fact that no language I know of
            -- makes modeling precise subtypes of strings straightforward
            -- (though perhaps refinement types would be good for this?) I'm not
            -- going to overthink it here.
  = Record5 { lastName :: String
            , firstName :: String
            , gender :: Gender
            , favoriteColor :: Color
            , dateOfBirth :: Day }
