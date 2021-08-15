module Arbitrary where

import OsString

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers

instance Arbitrary OsString where
  arbitrary = toOsString <$> arbitrary

instance EqProp OsString where
  (=-=) = eq
