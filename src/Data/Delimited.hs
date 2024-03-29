-- | The approach here is neither as performant as it might be, nor does it
-- | produce nice errors; nor does it, with elegance, deduplicate the syntax as
-- | expressed by printing and by parsing (a la invertible syntax descriptions).
-- | But it is simple.
module Data.Delimited
  (Delimited(..), parseDelimitedLine, parseDelimited, printDelimited)
where

import Data.Delimited.Delimiter (Delimiter, delimStr, delimName)
import Data.Delimited.Field (Field, parseField)
import Data.List (intersperse)
import Data.List.Utils (split)
import Prelude hiding (fail)

newtype Delimited = Delimited [Field]

parseDelimitedLine :: Delimiter -> String -> Either String Delimited
parseDelimitedLine delim text = do
  let rawFields = split (delimStr delim) text
  fields <- case traverse parseField rawFields of
    Nothing -> Left $ "Failed to parse " ++ delimName delim ++ "-delimited data"
    Just x -> return x
  return $ Delimited fields

printDelimitedLine :: Delimiter -> Delimited -> String
printDelimitedLine delim (Delimited fields) =
  concat $ intersperse (delimStr delim) $ fmap show fields

parseDelimited :: Delimiter -> String -> Either String [Delimited]
parseDelimited delim =
  traverse (parseDelimitedLine delim) . filter (/= "") . lines

printDelimited :: Delimiter -> [Delimited] -> String
printDelimited delim records =
  unlines $ fmap (printDelimitedLine delim) records
