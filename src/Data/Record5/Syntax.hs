module Data.Record5.Syntax (printRecord5, parseRecord5) where

--import Data.Colour.Names (readColourName)
import Data.Delimited.Delimiter (Delimiter)
import Data.Record5
--import Data.Time (parseTimeM, formatTime, defaultTimeLocale)

_printGender :: Gender -> String
_printGender Male = "M"
_printGender Female = "F"

_parseGender :: String -> Maybe Gender
_parseGender "M" = Just Male
_parseGender "F" = Just Female
_parseGender _ = Nothing

_dateFormat :: String
_dateFormat = "%m/%d/%Y"

-- readColourName fColor
-- parseTimeM False defaultTimeLocale dateFormat dateOB

-- We encapsulate the use of invertible syntax descriptions within this module
parseRecord5 :: Delimiter -> String -> IO Record5
parseRecord5 _delim _text = undefined -- TODO

-- We encapsulate the use of invertible syntax descriptions within this module
printRecord5 :: Delimiter -> Record5 -> IO String
printRecord5 _delim = undefined -- TODO
