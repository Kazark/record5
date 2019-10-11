{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-} -- A pleasant, plain extension
module Main where

import Control.UserFacingError (fail)
import Data.Record5 (parseDelim, Delimiter)
import Prelude hiding (fail)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

badCliArgs :: String
badCliArgs =
  "Invalid command-line arguments; expected exactly one, the input file."

unrecognizedExt :: String -> String
unrecognizedExt ext =
  -- The (x :: Delimiter) below is why we have ScopedTypeVariables turned on
  let supported = unwords [show x | (x :: Delimiter) <- [minBound..maxBound]] in
  "Unrecognized file extension: " ++ ext ++ " (supported: " ++ supported ++ ")."

determineFormat :: FilePath -> IO Delimiter
determineFormat inputFile =
  let ext = takeExtension inputFile
  in case parseDelim ext of
    Nothing -> fail $ unrecognizedExt ext
    Just delimiter -> return delimiter

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
  delimiter <- determineFormat inputFile
  putStrLn "I don't actually work yet. Come back later. ;)"
