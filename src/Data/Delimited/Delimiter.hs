module Data.Delimited.Delimiter where

import Control.UserFacingError (fail)
import Prelude hiding (fail)
import System.FilePath (takeExtension)

data Delimiter
  = Comma | Pipe | Space
  deriving (Eq, Bounded, Ord, Enum)

delimChar :: Delimiter -> Char
delimChar Comma = ','
delimChar Pipe = '|'
delimChar Space = ' '

delimName :: Delimiter -> String
delimName Comma = "comma"
delimName Pipe = "pipe"
delimName Space = "space"

instance Show Delimiter where
  show d = [delimChar d]

delimiters :: [Delimiter]
delimiters = [minBound..maxBound]

-- | Establish the convention for file extensions by delimiter.
extension :: Delimiter -> String
-- This is a standard extension,
extension Comma = ".csv"
-- but these two are not, as far as I know.
extension Pipe = ".psv"
extension Space = ".ssv"

-- | Determine the delimiter based on the convention for file extensions.
delimFromExt :: String -> Maybe Delimiter
delimFromExt s = lookup s tab where
  -- A little trick I learned from https://stackoverflow.com/a/53645493/834176
  -- to ensure that the parser and printer stay in sync. If this were a large
  -- datatype or frequently used, we might want to ensure this table was only
  -- calculated once. But it's neither large nor frequently used.
  tab = [(extension x, x) | x <- delimiters]

unrecognizedExt :: String -> String
unrecognizedExt ext =
  -- The (x :: Delimiter) below is why we have ScopedTypeVariables turned on
  let supported = unwords [extension x | x <- delimiters] in
  -- Because we've dynamically calculated the supported formats, our error
  -- message will never be out of sync with our code---the compiler will ensure
  -- it! One of the important things when designing code is to consider the
  -- forces of change over time. You don't know what will change in the future,
  -- and you don't want to clutter your code with guesses about the future, but
  -- you should be aware that things will change unless your codebase dies!
  "Unrecognized file extension: " ++ ext ++ " (supported: " ++ supported ++ ")."

determineFormat :: FilePath -> IO Delimiter
determineFormat inputFile =
  let ext = takeExtension inputFile
  in case delimFromExt ext of
    Nothing -> fail $ unrecognizedExt ext
    Just delimiter -> return delimiter
