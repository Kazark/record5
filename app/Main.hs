{-# LANGUAGE LambdaCase #-} -- A pleasant, plain extension
module Main where

-- I prefer fully explicit imports. I believe it makes your code more readable
-- by informing people as to where each value or type is coming from. If someone
-- is reading your code in an editor that supports "go to definition," this is
-- not strictly necessary; but I like to not assume that they are.
import Control.UserFacingError (fail)
import Data.Delimited.Delimiter (determineFormat)
import Data.Delimited (printDelimited, parseDelimited)
import Data.Record5.Syntax (printRecord5, parseRecord5)
import Prelude hiding (fail)
import System.Environment (getArgs)

badCliArgs :: String
badCliArgs =
  "Invalid command-line arguments; expected exactly one, the input file."

-- | FilePath is an imprecise type in Haskell; it's merely an alias for a
-- | string. However, we don't have that much to do with file paths here, so
-- | we'll just roll with it, rather than pulling in a more precisely typed path
-- | library.
parseArgs :: [String] -> IO FilePath
parseArgs = \case
  [x] -> return x
  _ -> fail badCliArgs

main :: IO ()
main = do
  cliArgs <- getArgs
  inputFile <- parseArgs cliArgs
  -- If we expected that the input file might be huge, we might want to stream
  -- it in. But this is just an exercise. :)
  text <- readFile inputFile
  -- In CSV files, newlines are significant. Assuming that's what we want here.
  delimiter <- determineFormat inputFile
  delimited <- parseDelimited delimiter text
  records <- traverse parseRecord5 delimited
  let printed = printDelimited delimiter $ map printRecord5 records
  putStrLn printed
