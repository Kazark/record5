module Data.Record5.Syntax (printRecord5, parseRecord5) where

import Data.Function ((&))
--import Data.Colour.Names (readColourName)
import Data.Delimited.Field (avowField)
import Data.Delimited (Delimited(..))
import Data.Record5
import Data.Time ({-parseTimeM, -}formatTime, defaultTimeLocale)

printGender :: Gender -> String
printGender Male = "M"
printGender Female = "F"

_parseGender :: String -> Maybe Gender
_parseGender "M" = Just Male
_parseGender "F" = Just Female
_parseGender _ = Nothing

dateFormat :: String
dateFormat = "%m/%d/%Y"

-- readColourName fColor
-- parseTimeM False defaultTimeLocale dateFormat dateOB

parseRecord5 :: Delimited -> IO Record5
parseRecord5 _text = undefined -- TODO

printRecord5 :: Record5 -> Delimited
printRecord5 record =
  [ lastName record
  , firstName record
  -- Because Gender is only M or F, it is safe to avow it is a field (because it
  -- will never contain a comma, space or pipe).
  , avowField $ printGender $ gender record
  , favoriteColor record
  -- Because of the format we've supplied, and the highly limited nature of date
  -- data, it will never contain a comma, space or pipe; and therefore the avow
  -- is safe.
  , avowField $ formatTime defaultTimeLocale dateFormat $ dateOfBirth record
  ] & Delimited
