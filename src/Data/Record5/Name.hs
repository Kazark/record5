module Data.Record5.Name (Name, parseName) where

import Data.Char (isAlpha)

-- | An opaque newtype, conceptually a subtype of string, that lets us establish
-- | names as alphabetic.
newtype Name = Name String

instance Show Name where
  show (Name n) = n

-- | Construct a name from the string if all characters are alphabetic,
-- | according to the Unicode definition of alphabetic.
parseName :: String -> Maybe Name
parseName name =
  if all isAlpha name
  then Just $ Name name
  else Nothing
