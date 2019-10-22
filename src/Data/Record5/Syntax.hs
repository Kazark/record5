module Data.Record5.Syntax (printRecords, parseRecords) where

import Data.Function ((&))
import Data.Delimited.Delimiter (Delimiter)
import Data.Delimited.Field (avowField)
import Data.Delimited (Delimited(..), parseDelimited, printDelimited)
import Data.Record5
import Data.Time (parseTimeM, formatTime, defaultTimeLocale)

printGender :: Gender -> String
printGender Male = "M"
printGender Female = "F"

parseGender :: String -> Maybe Gender
parseGender "M" = Just Male
parseGender "F" = Just Female
parseGender _ = Nothing

dateFormat :: String
dateFormat = "%m/%d/%Y"

parseRecord5 :: Delimited -> IO Record5
parseRecord5 (Delimited [lastN, firstN, rawGender, fColor, rawDOB]) = do
  gender' <- case parseGender $ show rawGender of
               Nothing -> fail $ "Unrecognized gender: " ++ show rawGender
               Just g -> return g
  dob <- parseTimeM False defaultTimeLocale dateFormat $ show rawDOB
  return $ Record5 lastN firstN gender' fColor dob
parseRecord5 (Delimited fields) =
  fail $ "Expected 5 fields, got " ++ show (length fields)

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

parseRecords :: Delimiter -> String -> IO [Record5]
parseRecords delimiter text = do
  delimited <- parseDelimited delimiter text
  traverse parseRecord5 delimited

printRecords :: Delimiter -> [Record5] -> String
printRecords delimiter = printDelimited delimiter . fmap printRecord5
