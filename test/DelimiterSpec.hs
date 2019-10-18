{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DelimiterSpec (tests) where

import Data.DeriveTH
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Delimited.Delimiter
import Data.List (isInfixOf)

$(derive makeArbitrary ''Delimiter)

tests :: TestTree
tests = testGroup "Delimiter type"
 [ testProperty "Determining the delimiter from the file extension is left-inverse to determining the extension from the delimiter" $ \d ->
      delimFromExt (extension d) == Just d
  , testProperty "The unrecognized extension error message lists all supported extensions" $ \(ext, d) ->
      isInfixOf (extension d) (unrecognizedExt ext)
  ]
