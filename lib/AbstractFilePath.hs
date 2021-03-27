module AbstractFilePath
  (
  -- * Types
    AbstractFilePath
  , ByteStringStrategy

  -- * construction
  , toAbstractFilePath
  , fromByteString
  , fromByteStringUnsafe

  -- * deconstruction
  , fromAbstractFilePath
  , toByteString
  )
where

import AbstractFilePath.Internal ( AbstractFilePath, ByteStringStrategy (..), toAbstractFilePath, fromAbstractFilePath, fromByteString, fromByteStringUnsafe, toByteString )
