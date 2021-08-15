{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.ByteString.Short
  (

  -- * Introducing and eliminating 'ShortByteString's
  empty,
  singleton,
  pack,
  unpack,
  fromShort,
  toShort,

  -- * Basic interface
  snoc,
  cons,
  append,
  last,
  tail,
  head,
  init,
  null,
  length,

  -- * Transforming ShortByteStrings
  map,
  intercalate,


  -- * Reducing 'ShortByteString's (folds)
  foldl',

  -- ** Special folds
  all,
  any,
  concat,

  -- * Substrings

  -- ** Breaking strings
  take,
  drop,
  dropWhile,
  dropWhileEnd,
  breakEnd,
  break,
  span,
  spanEnd,
  splitAt,
  split,
  splitWith,
  stripSuffix,

  -- * Predicates
  isInfixOf,
  isPrefixOf,
  isSuffixOf,

  -- ** Search for arbitrary substrings
  breakSubstring,

  -- * Searching ShortByteStrings

  -- ** Searching by equality
  elem,

  -- * Indexing ShortByteStrings
  index,
  elemIndex,
  findIndex,

  -- * Low level conversions
  -- ** Packing 'CString's and pointers
  packCString,
  packCStringLen,

  -- ** Using ShortByteStrings as 'CString's
  useAsCString,
  useAsCStringLen,
  )
where

import Prelude hiding
    ( all
    , any
    , break
    , concat
    , drop
    , dropWhile
    , elem
    , head
    , init
    , last
    , length
    , map
    , null
    , span
    , splitAt
    , tail
    , take
    )

import "bytestring" Data.ByteString.Short
    ( ShortByteString
    , empty
    , fromShort
    , index
    , length
    , null
    , pack
    , packCString
    , packCStringLen
    , toShort
    , unpack
    , useAsCString
    , useAsCStringLen
    )
import Data.ByteString.Short.Internal
import "bytestring" Data.ByteString.Short.Internal
    ( ShortByteString (SBS), createFromPtr )
import qualified Data.Foldable as F
import Data.Word8

import qualified "bytestring" Data.ByteString.Short as BS
import qualified "bytestring" Data.ByteString.Short.Internal as BS


import Data.ByteString.Internal
    ( ByteString (..), accursedUnutterablePerformIO, c_strlen, memcmp )

import Data.Data
    ( Data (..), mkNoRepType )
import Data.Typeable
    ( Typeable )
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
    ( Semigroup ((<>)) )
#endif
import qualified Data.List as List
    ( intersperse, length )
import Data.Monoid
    ( Monoid (..) )
import Data.String
    ( IsString (..) )
import Foreign.C.String
    ( CString, CStringLen )
import Foreign.C.Types
    ( CInt (..), CSize (..) )
import Foreign.ForeignPtr
    ( touchForeignPtr )
import Foreign.ForeignPtr.Unsafe
    ( unsafeForeignPtrToPtr )
import Foreign.Marshal.Alloc
    ( allocaBytes, free, mallocBytes )
import Foreign.Ptr
import Foreign.Storable
    ( pokeByteOff )

import GHC.Exts
import GHC.ForeignPtr
    ( ForeignPtr (ForeignPtr), ForeignPtrContents (PlainPtr) )
import GHC.IO
import GHC.ST
    ( ST (ST), runST )
import GHC.Word


import Data.Bits
    ( FiniteBits (finiteBitSize), shiftL, (.&.), (.|.) )
import Debug.Trace


-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ShortByteString's

-- | /O(1)/ Convert a 'Word8' into a 'ShortByteString'
singleton :: Word8 -> ShortByteString
singleton = \w -> create 1 (\mba -> writeWord8Array mba 0 w)
{-# INLINE [1] singleton #-}


-- ---------------------------------------------------------------------
-- Basic interface

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ Append a byte to the end of a 'ShortByteString'
-- 
-- Note: copies the entire byte array
snoc :: Word8 -> ShortByteString -> ShortByteString
snoc c = \sbs -> let l = BS.length sbs
                     nl = l + 1
  in create nl $ \mba -> do
      copyByteArray (asBA sbs) 0 mba 0 nl
      writeWord8Array mba l c
{-# INLINE snoc #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
-- Note: copies the entire byte array
cons :: Word8 -> ShortByteString -> ShortByteString
cons c = \sbs -> let l = BS.length sbs + 1
  in create l $ \mba -> do
      writeWord8Array mba 0 c
      copyByteArray (asBA sbs) 0 mba 1 l
{-# INLINE cons #-}

-- | /O(n)/ Append two ShortByteStrings
append :: ShortByteString -> ShortByteString -> ShortByteString
append = mappend
{-# INLINE append #-}

-- | /O(1)/ Extract the last element of a ShortByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ShortByteString.
last :: ShortByteString -> Word8
last = \sbs -> case BS.null sbs of
  True -> error "empty ShortByteString"
  False -> indexWord8Array (asBA sbs) (BS.length sbs - 1)
{-# INLINE last #-}

-- | /O(n)/ Extract the elements after the head of a ShortByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- Note: copies the entire byte array
tail :: ShortByteString -> ShortByteString
tail = \sbs -> 
  let l = BS.length sbs
      nl = l - 1
  in case BS.null sbs of
      True -> error "empty ShortByteString"
      False -> create nl $ \mba -> copyByteArray (asBA sbs) 1 mba 0 nl
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a ShortByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ShortByteString.
head :: ShortByteString -> Word8
head = \sbs -> case BS.null sbs of
  True -> error "empty ShortByteString"
  False -> indexWord8Array (asBA sbs) 0
{-# INLINE head #-}

-- | /O(n)/ Return all the elements of a 'ShortByteString' except the last one.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- Note: copies the entire byte array
init :: ShortByteString -> ShortByteString
init = \sbs ->
  let l = BS.length sbs
      nl = l - 1
  in case BS.null sbs of
      True -> error "empty ShortByteString"
      False -> create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl
{-# INLINE init #-}


-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ShortByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ShortByteString -> ShortByteString
map f = BS.pack . fmap f . BS.unpack

-- | /O(n)/ The 'intercalate' function takes a 'ShortByteString' and a list of
-- 'ShortByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ShortByteString -> [ShortByteString] -> ShortByteString
intercalate s = concat . List.intersperse s
{-# INLINE [1] intercalate #-}


-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl' f v = F.foldl f v . BS.unpack
{-# INLINE foldl' #-}


-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Applied to a predicate and a 'ShortByteString', 'all' determines
-- if all elements of the 'ShortByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ShortByteString -> Bool
all k sbs = go 0
  where
    l = BS.length sbs
    ba = asBA sbs
    w = indexWord8Array ba
    go !n | n >= l    = True
          | otherwise = k (w n) && go (n + 1)


-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ShortByteString -> Bool
any k sbs = go 0
  where
    l = BS.length sbs
    ba = asBA sbs
    w = indexWord8Array ba
    go !n | n >= l    = False
          | otherwise = k (w n) || go (n + 1)
{-# INLINE [1] any #-}


-- | /O(n)/ Concatenate a list of ShortByteStrings.
concat :: [ShortByteString] -> ShortByteString
concat = mconcat


-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n)/ 'take' @n@, applied to a ShortByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- Note: copies the entire byte array
take :: Int -> ShortByteString -> ShortByteString
take = \n -> \sbs ->
  let len = min (BS.length sbs) (max 0 n)
  in create len $ \mba -> copyByteArray (asBA sbs) 0 mba 0 len
{-# INLINE take #-}

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or @[]@ if @n > 'length' xs@.
--
-- Note: copies the entire byte array
drop  :: Int -> ShortByteString -> ShortByteString
drop = \n -> \sbs ->
  let len = max 0 (BS.length sbs - max 0 n)
  in create len $ \mba -> copyByteArray (asBA sbs) n mba 0 len
{-# INLINE drop #-}

-- | Similar to 'P.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
--
-- Note: copies the entire byte array
dropWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhile f ps = drop (findIndexOrLength (not . f) ps) ps

-- | Similar to 'P.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhileEnd f ps = take (findFromEndUntil (not . f) ps) ps
{-# INLINE dropWhileEnd #-}

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
breakEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
breakEnd p sbs = splitAt (findFromEndUntil p sbs) sbs
{-# INLINE breakEnd #-}

-- | Similar to 'P.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
break :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
break = \p -> \ps -> case findIndexOrLength p ps of n -> (take n ps, drop n ps)
{-# INLINE [1] break #-}

-- | Similar to 'P.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
span p = break (not . p)

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('takeWhileEnd' p &&& 'dropWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse ps) in (reverse y, reverse x)
--
spanEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
spanEnd  p ps = splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
--
-- Note: copies the substrings
splitAt :: Int -> ShortByteString -> (ShortByteString, ShortByteString)
splitAt n xs
  | n <= 0 = (mempty, xs)
  | n >= BS.length xs = (xs, mempty)
  | otherwise = (take n xs, drop n xs)

-- | /O(n)/ Break a 'ShortByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- Note: copies the substrings
split :: Word8 -> ShortByteString -> [ShortByteString]
split w = splitWith (== w)


-- | /O(n)/ Splits a 'ShortByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word8 -> Bool) -> ShortByteString -> [ShortByteString]
splitWith p sbs
  | BS.null sbs = []
  | otherwise = go sbs
  where
    go sbs'
      | BS.null sbs' = [mempty]
      | otherwise =
          case break p sbs' of
            (a, b)
              | BS.null b -> [a]
              | otherwise -> a : go (tail b)


-- | /O(n)/ The 'stripSuffix' function takes two ShortByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripSuffix sbs1 sbs2 = do
  let l1 = BS.length sbs1
      l2 = BS.length sbs2
  if | l1 == 0   -> Just sbs2
     | l2 < l1   -> Nothing
     | otherwise -> unsafeDupablePerformIO $ do
         p1 <- mallocBytes l1
         p2 <- mallocBytes l2
         BS.copyToPtr sbs1 0 p1 l1
         BS.copyToPtr sbs2 0 p2 l2
         i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
         if i == 0
          then do
            sbs <- createFromPtr p2 (fromIntegral (l2 - l1))
            free p1
            free p2
            return $! Just sbs
          else do
            free p1
            free p2
            return Nothing


-- --------------------------------------------------------------------
-- Predicates

-- | Check whether one string is a substring of another.
isInfixOf :: ShortByteString -> ShortByteString -> Bool
isInfixOf p s = BS.null p || not (BS.null $ snd $ breakSubstring p s)

-- |/O(n)/ The 'isPrefixOf' function takes two ShortByteStrings and returns 'True'
isPrefixOf :: ShortByteString -> ShortByteString -> Bool
isPrefixOf sbs1 sbs2 = do
  let l1 = BS.length sbs1
      l2 = BS.length sbs2
  if | l1 == 0   -> True
     | l2 < l1   -> False
     | otherwise -> unsafeDupablePerformIO $ do
         p1 <- mallocBytes l1
         p2 <- mallocBytes l2
         BS.copyToPtr sbs1 0 p1 l1
         BS.copyToPtr sbs2 0 p2 l2
         i <- memcmp p1 p2 (fromIntegral l1)
         free p1
         free p2
         return $! i == 0

-- | /O(n)/ The 'isSuffixOf' function takes two ShortByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
isSuffixOf :: ShortByteString -> ShortByteString -> Bool
isSuffixOf sbs1 sbs2 = do
  let l1 = BS.length sbs1
      l2 = BS.length sbs2
  if | l1 == 0   -> True
     | l2 < l1   -> False
     | otherwise -> unsafeDupablePerformIO $ do
         p1 <- mallocBytes l1
         p2 <- mallocBytes l2
         BS.copyToPtr sbs1 0 p1 l1
         BS.copyToPtr sbs2 0 p2 l2
         i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
         free p1
         free p2
         return $! i == 0

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
--
breakSubstring :: ShortByteString -- ^ String to search for
               -> ShortByteString -- ^ String to search in
               -> (ShortByteString, ShortByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat =
  case lp of
    0 -> (mempty,)
    1 -> breakByte (head pat)
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
             then shift
             else karpRabin
  where
    lp = BS.length pat
    karpRabin :: ShortByteString -> (ShortByteString, ShortByteString)
    karpRabin src
        | BS.length src < lp = (src,mempty)
        | otherwise = search (rollingHash $ take lp src) lp
      where
        k           = 2891336453 :: Word32
        rollingHash = foldl' (\h b -> h * k + fromIntegral b) 0
        hp          = rollingHash pat
        m           = k ^ lp
        get = fromIntegral . BS.unsafeIndex src
        search !hs !i
            | hp == hs && pat == take lp b = u
            | BS.length src <= i           = (src, mempty) -- not found
            | otherwise                    = search hs' (i + 1)
          where
            u@(_, b) = splitAt (i - lp) src
            hs' = hs * k +
                  get i -
                  m * get (i - lp)
    {-# INLINE karpRabin #-}

    shift :: ShortByteString -> (ShortByteString, ShortByteString)
    shift !src
        | BS.length src < lp = (src, mempty)
        | otherwise       = search (intoWord $ take lp src) lp
      where
        intoWord :: ShortByteString -> Word
        intoWord = foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
        wp   = intoWord pat
        mask = (1 `shiftL` (8 * lp)) - 1
        search !w !i
            | w == wp            = splitAt (i - lp) src
            | BS.length src <= i = (src, mempty)
            | otherwise       = search w' (i + 1)
          where
            b  = fromIntegral (BS.unsafeIndex src i)
            w' = mask .&. ((w `shiftL` 8) .|. b)
    {-# INLINE shift #-}


-- --------------------------------------------------------------------
-- Searching ShortByteString

-- | /O(n)/ 'elem' is the 'ShortByteString' membership predicate.
elem :: Word8 -> ShortByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True


-- --------------------------------------------------------------------
-- Indexing ShortByteString

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ShortByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Word8 -> ShortByteString -> Maybe Int
elemIndex k = findIndex (==k)
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'ShortByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ShortByteString -> Maybe Int
findIndex k sbs = go 0
  where
    l = BS.length sbs
    ba = asBA sbs
    w = indexWord8Array ba
    go !n | n >= l    = Nothing
          | k (w n)   = Just n
          | otherwise = go (n + 1)
{-# INLINE findIndex #-}


-- --------------------------------------------------------------------
-- Internal

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ShortByteString -> Int
findFromEndUntil k sbs = go (BS.length sbs - 1)
  where
    ba = asBA sbs
    w = indexWord8Array ba
    go !n | n < 0     = 0
          | k (w n)   = n + 1
          | otherwise = go (n - 1)
{-# INLINE findFromEndUntil #-}

findIndexOrLength :: (Word8 -> Bool) -> ShortByteString -> Int
findIndexOrLength k sbs = go 0
  where
    l = BS.length sbs
    ba = asBA sbs
    w = indexWord8Array ba
    go !n | n >= l    = l
          | k (w n)   = n
          | otherwise = go (n + 1)
{-# INLINE findIndexOrLength #-}


breakByte :: Word8 -> ShortByteString -> (ShortByteString, ShortByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p, mempty)
    Just n  -> (take n p, drop n p)
{-# INLINE breakByte #-}

indexWord8Array :: BA -> Int -> Word8
indexWord8Array (BA# ba#) (I# i#) = W8# (indexWord8Array# ba# i#)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W8# w#) =
  ST $ \s -> case writeWord8Array# mba# i# w# s of
               s -> (# s, () #)
