{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FieldSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Delimited.Delimiter (delimChar)
import Data.Delimited.Field (parseField)
import DelimiterSpec ()

tests :: TestTree
tests = testGroup "Field type"
  [ testProperty "No delimiter is valid within a field" $ \d ->
      parseField [delimChar d] == Nothing
  ]
