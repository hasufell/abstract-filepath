module Arbitrary where

import AFP.OsString
import AFP.OsString.Internal.Types
import qualified AFP.OsString.Posix as Posix
import qualified AFP.OsString.Windows as Windows

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers

instance Arbitrary OsString where
  arbitrary = toOsString <$> arbitrary

instance EqProp OsString where
  (=-=) = eq

instance Arbitrary PosixString where
  arbitrary = Posix.toPlatformString <$> arbitrary

instance EqProp PosixString where
  (=-=) = eq

instance Arbitrary WindowsString where
  arbitrary = Windows.toPlatformString <$> arbitrary

instance EqProp WindowsString where
  (=-=) = eq
