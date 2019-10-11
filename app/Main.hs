{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-} -- A pleasant, plain extension
module Main where

-- I prefer fully explicit imports. I believe it makes your code more readable
-- by informing people as to where each value or type is coming from. If someone
-- is reading your code in an editor that supports "go to definition," this is
-- not strictly necessary; but I like to not assume that they are.
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
