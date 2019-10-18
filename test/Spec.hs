module Main where

import qualified DelimiterSpec
import qualified FieldSpec
import           Test.Tasty

main :: IO ()
main = defaultMain $
  testGroup "Unit tests" [ FieldSpec.tests
                         , DelimiterSpec.tests
                         ]
