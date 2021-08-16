module Arbitrary where

import AFP.OsString

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers

instance Arbitrary OsString where
  arbitrary = toOsString <$> arbitrary

instance EqProp OsString where
  (=-=) = eq
