module Data.Delimited.Field (Field, parseField, avowField) where

import Data.Maybe (fromJust)
import qualified Data.Delimited.Delimiter as Delim

-- | An opaque newtype, conceptually a subtype of string, that blocks the use of
-- | delimiter characters.
newtype Field = Field String deriving Eq

instance Show Field where
  show (Field f) = f

delimiters :: [Char]
delimiters = fmap Delim.delimChar Delim.delimiters

-- | We'll make this simple and say:
-- | 1. No matter which delimiter you are using, we expect exactly one to
-- |    separate two fields.
-- | 2. We do not expect any of the fields to contain an active delimiter.
-- | 3. We do not support mixing delimiters, including having extra spaces
-- |    around pipes or commas.
parseField :: String -> Maybe Field
parseField field =
  -- Technically O(n*m), but m=3 so that only counts as O(n)...
  if all (\c -> not $ elem c delimiters) field
  then Just $ Field field
  else Nothing

-- | Only use this function where you have proof that the string will never be
-- | an invalid field. This is an unsafe function, in that it will crash on bad
-- | input; there are exotic ways to let the typechecker in on what we're trying
-- | to do here, but I'm going to rely on property-based tests to prove safety
-- | instead.
avowField :: String -> Field
avowField = fromJust . parseField
