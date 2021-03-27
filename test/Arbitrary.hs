module Arbitrary where

import AbstractFilePath

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers

instance Arbitrary AbstractFilePath where
  arbitrary = toAbstractFilePath <$> arbitrary

instance EqProp AbstractFilePath where
  (=-=) = eq
