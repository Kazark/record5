-- | The approach here is neither as performant as it might be, nor does it
-- | produce nice errors; nor does it, with elegance, deduplicate the syntax as
-- | expressed by printing and by parsing (a la invertible syntax descriptions).
-- | But it is simple.
module Data.Delimited (Delimited(..), parseDelimited, printDelimited) where

import Data.Delimited.Field (Field, parseField)
import Data.Delimited.Delimiter (Delimiter, delimChar)
import Data.List.Utils (split)
import Data.List (intersperse)

data Delimited = Delimited [Field]

parseDelimitedLine :: Delimiter -> String -> Maybe Delimited
parseDelimitedLine delim text = do
  let rawFields = split [delimChar delim] text
  fields <- traverse parseField rawFields
  return $ Delimited fields

printDelimitedLine :: Delimiter -> Delimited -> String
printDelimitedLine delim (Delimited fields) =
  concat $ intersperse [delimChar delim] $ fmap show fields

parseDelimited :: Delimiter -> String -> Maybe [Delimited]
parseDelimited delim text =
  traverse (parseDelimitedLine delim) $ lines text

printDelimited :: Delimiter -> [Delimited] -> String
printDelimited delim records =
  unlines $ fmap (printDelimitedLine delim) records
