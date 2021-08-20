{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module AFP.Data.ByteString.Short.Word16 (

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
  foldl,
  foldl',
  foldl1,
  foldl1',

  foldr,
  foldr',
  foldr1,
  foldr1',

  -- ** Special folds
  all,
  any,
  concat,

  -- ** Generating and unfolding ByteStrings
  replicate,
  unfoldr,
  unfoldrN,

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

  -- ** Searching with a predicate
  find,
  filter,
  partition,

  -- * Indexing ShortByteStrings
  index,
  elemIndex,
  elemIndices,
  findIndex,
  findIndices,

  -- * Low level conversions
  -- ** Packing 'CString's and pointers
  packCWString,
  packCWStringLen,
  newCWString,

  -- ** Using ShortByteStrings as 'CString's
  useAsCWString,
  useAsCWStringLen,
  )
where

import AFP.Data.ByteString.Short
    ( append, intercalate, isInfixOf, isPrefixOf, isSuffixOf, stripSuffix, fromShort, toShort, concat, breakSubstring )
import AFP.Data.ByteString.Short.Internal
import qualified Data.ByteString.Short.Internal as BS
import AFP.Data.Word16

import Data.Bifunctor
    ( first, bimap )
import Prelude hiding
    ( all
    , any
    , break
    , concat
    , drop
    , dropWhile
    , elem
    , filter
    , foldl
    , foldl1
    , foldr
    , foldr1
    , head
    , init
    , last
    , length
    , map
    , null
    , replicate
    , span
    , splitAt
    , tail
    , take
    )
import GHC.List (errorEmptyList)
import qualified Data.Foldable as Foldable
import Data.ByteString.Short
    ( ShortByteString
    , empty
    , index
    , null
    )
import GHC.Exts
import GHC.Word
import GHC.ST
    ( ST (ST) )

import qualified Data.ByteString.Short as BS
import qualified Data.List as List


-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ShortByteString's

-- | /O(1)/ Convert a 'Word16' into a 'ShortByteString'
singleton :: Word16 -> ShortByteString
singleton = \w -> create 2 (\mba -> writeWord16Array mba 0 w)
{-# INLINE [1] singleton #-}


-- | /O(n)/. Convert a list into a 'ShortByteString'
pack :: [Word16] -> ShortByteString
pack = packBytes


-- | /O(n)/. Convert a 'ShortByteString' into a list.
unpack :: ShortByteString -> [Word16]
unpack = unpackBytes . assertEven


-- ---------------------------------------------------------------------
-- Basic interface

-- | This is the number of 'Word16', not 'Word8'.
length :: ShortByteString -> Int
length = (`div` 2) . BS.length . assertEven

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ Append a Word16 to the end of a 'ShortByteString'
-- 
-- Note: copies the entire byte array
snoc :: ShortByteString -> Word16 -> ShortByteString
snoc = \(assertEven -> sbs) c -> let l = length sbs
                                     nl = l + 1
                                     nl8 = nl * 2
  in create nl8 $ \mba -> do
      copyByteArray (asBA sbs) 0 mba 0 nl8
      writeWord16Array mba l c
{-# INLINE snoc #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
-- Note: copies the entire byte array
cons :: Word16 -> ShortByteString -> ShortByteString
cons c = \(assertEven -> sbs) -> let l = length sbs
                                     nl = l + 1
                                     nl8 = nl * 2
  in create nl8 $ \mba -> do
      writeWord16Array mba 0 c
      copyByteArray (asBA sbs) 0 mba 2 nl8
{-# INLINE cons #-}

-- | /O(1)/ Extract the last element of a ShortByteString, which must be finite and at least one Word16.
-- An exception will be thrown in the case of an empty ShortByteString.
last :: ShortByteString -> Word16
last = \(assertEven -> sbs) ->
  indexWord16Array (asBA sbs) (length sbs - 1)
{-# INLINE last #-}

-- | /O(n)/ Extract the elements after the head of a ShortByteString, which must at least one Word16.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- Note: copies the entire byte array
tail :: ShortByteString -> ShortByteString
tail = \(assertEven -> sbs) -> 
  let l = length sbs
      nl = l - 1
      nl8 = nl * 2
  in create nl8 $ \mba -> copyByteArray (asBA sbs) 2 mba 0 nl8
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a ShortByteString, which must be at least one Word16.
-- An exception will be thrown in the case of an empty ShortByteString.
head :: ShortByteString -> Word16
head = \(assertEven -> sbs) -> indexWord16Array (asBA sbs) 0
{-# INLINE head #-}

-- | /O(n)/ Return all the elements of a 'ShortByteString' except the last one.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- Note: copies the entire byte array
init :: ShortByteString -> ShortByteString
init = \(assertEven -> sbs) ->
  let l = length sbs
      nl = l - 1
      nl8 = nl * 2
   in create nl8 $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl8
{-# INLINE init #-}


-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ShortByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word16 -> Word16) -> ShortByteString -> ShortByteString
map f (assertEven -> sbs) =
    let l = length sbs
        ba = asBA sbs
    in create (l * 2) (\mba -> go ba mba 0 l)
  where
    go :: BA -> MBA s -> Int -> Int -> ST s ()
    go !ba !mba !i !l
      | i >= l = return ()
      | otherwise = do
          let w = indexWord16Array ba i
          writeWord16Array mba i (f w)
          go ba mba (i+1) l


-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Applied to a predicate and a 'ShortByteString', 'all' determines
-- if all elements of the 'ShortByteString' satisfy the predicate.
all :: (Word16 -> Bool) -> ShortByteString -> Bool
all k (assertEven -> sbs) = go 0
  where
    l = length sbs
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n >= l = True
          | otherwise = k (w n) && go (n + 1)


-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word16 -> Bool) -> ShortByteString -> Bool
any k (assertEven -> sbs) = go 0
  where
    l = length sbs
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n >= l = False
          | otherwise = k (w n) || go (n + 1)
{-# INLINE [1] any #-}


-- ---------------------------------------------------------------------
-- Unfolds and replicates


-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
replicate :: Int -> Word16 -> ShortByteString
replicate w c
    | w <= 0    = empty
    | otherwise = create (w * 2) (\mba -> go mba 0)
  where
    go mba ix
      | ix < 0 || ix >= w = pure ()
      | otherwise = writeWord16Array mba ix c >> go mba (ix + 1)
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ShortByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ShortByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word16, a)) -> a -> ShortByteString
unfoldr f x0 = packBytesRev $ go x0 mempty
 where
   go x words' = case f x of
                    Nothing -> words'
                    Just (w, x') -> go x' (w:words')
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ShortByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: Int -> (a -> Maybe (Word16, a)) -> a -> (ShortByteString, Maybe a)
unfoldrN i f x0 = first packBytesRev $ go i x0 mempty
 where
   go i' x words'
    | i' < 0     = (words', Just x)
    | otherwise = case f x of
                    Nothing -> (words', Nothing)
                    Just (w, x') -> go (i' - 1) x' (w:words')
{-# INLINE unfoldrN #-}



-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n)/ 'take' @n@, applied to a ShortByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- Note: copies the entire byte array
take :: Int  -- ^ number of Word16
     -> ShortByteString
     -> ShortByteString
take = \n -> \(assertEven -> sbs) ->
  let len = min (length sbs) (max 0 n )
      len8 = len * 2
  in create len8 $ \mba -> copyByteArray (asBA sbs) 0 mba 0 len8
{-# INLINE take #-}

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or @[]@ if @n > 'length' xs@.
--
-- Note: copies the entire byte array
drop  :: Int -> ShortByteString -> ShortByteString
drop = \n -> \(assertEven -> sbs) ->
  let len = max 0 (length sbs - max 0 n)
      len8 = len * 2
  in create len8 $ \mba -> copyByteArray (asBA sbs) (n * 2) mba 0 len8
{-# INLINE drop #-}

-- | Similar to 'P.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
--
-- Note: copies the entire byte array
dropWhile :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
dropWhile f (assertEven -> ps) = drop (findIndexOrLength (not . f) ps) ps

-- | Similar to 'P.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
dropWhileEnd f (assertEven -> ps) = take (findFromEndUntil (not . f) ps) ps
{-# INLINE dropWhileEnd #-}

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
breakEnd :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
breakEnd p (assertEven -> sbs) = splitAt (findFromEndUntil p sbs) sbs
{-# INLINE breakEnd #-}

-- | Similar to 'P.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
break :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
break = \p -> \(assertEven -> ps) -> case findIndexOrLength p ps of n -> (take n ps, drop n ps)
{-# INLINE [1] break #-}

-- | Similar to 'P.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
span p = break (not . p) . assertEven

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
spanEnd :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
spanEnd  p (assertEven -> ps) = splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
--
-- Note: copies the substrings
splitAt :: Int -- ^ number of Word16
        -> ShortByteString
        -> (ShortByteString, ShortByteString)
splitAt n (assertEven -> xs)
  | n <= 0 = (mempty, xs)
  | n >= BS.length xs * 2 = (xs, mempty)
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
split :: Word16 -> ShortByteString -> [ShortByteString]
split w = splitWith (== w) . assertEven


-- | /O(n)/ Splits a 'ShortByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word16 -> Bool) -> ShortByteString -> [ShortByteString]
splitWith p (assertEven -> sbs)
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


-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ShortByteString, reduces the
-- ShortByteString using the binary operator, from left to right.
--
foldl :: (a -> Word16 -> a) -> a -> ShortByteString -> a
foldl f v = List.foldl f v . unpack . assertEven
{-# INLINE foldl #-}

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word16 -> a) -> a -> ShortByteString -> a
foldl' f v = List.foldl' f v . unpack . assertEven
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ShortByteString,
-- reduces the ShortByteString using the binary operator, from right to left.
foldr :: (Word16 -> a -> a) -> a -> ShortByteString -> a
foldr f v = List.foldr f v . unpack . assertEven
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
foldr' :: (Word16 -> a -> a) -> a -> ShortByteString -> a
foldr' k v = Foldable.foldr' k v . unpack . assertEven
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ShortByteString's.
-- An exception will be thrown in the case of an empty ShortByteString.
foldl1 :: (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldl1 k = List.foldr1 k . unpack . assertEven
{-# INLINE foldl1 #-}

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty ShortByteString.
foldl1' :: (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldl1' k = List.foldl1' k . unpack . assertEven

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ShortByteString's
-- An exception will be thrown in the case of an empty ShortByteString.
foldr1 :: (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldr1 k = List.foldr1 k . unpack . assertEven
{-# INLINE foldr1 #-}

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: (Word16 -> Word16 -> Word16) -> ShortByteString -> Word16
foldr1' k = _foldr1' k . unpack . assertEven
 where
  -- | A strict version of 'foldr1'.
  _foldr1'                  :: (a -> a -> a) -> [a] -> a
  _foldr1' f (x:xs)         =  Foldable.foldr' f x xs
  _foldr1' _ []             =  errorEmptyList "foldl1'"
{-# INLINE foldr1' #-}



-- --------------------------------------------------------------------
-- Searching ShortByteString

-- | /O(n)/ 'elem' is the 'ShortByteString' membership predicate.
elem :: Word16 -> ShortByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word16 -> Bool) -> ShortByteString -> ShortByteString
filter k sbs
    | null sbs   = sbs
    | otherwise = pack . List.filter k . unpack . assertEven $ sbs
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word16 -> Bool) -> ShortByteString -> Maybe Word16
find f = List.find f . unpack . assertEven
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word16 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
partition f s
    | null s    = (s, s)
    | otherwise = bimap pack pack . List.partition f . unpack . assertEven $ s

-- --------------------------------------------------------------------
-- Indexing ShortByteString

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ShortByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Word16 -> ShortByteString -> Maybe Int
elemIndex k = findIndex (==k)
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Word16 -> ShortByteString -> [Int]
elemIndices k = findIndices (==k)

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'ShortByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word16 -> Bool) -> ShortByteString -> Maybe Int
findIndex k sbs = go 0
  where
    l = length sbs
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n >= l    = Nothing
          | k (w n)   = Just n
          | otherwise = go (n + 1)
{-# INLINE findIndex #-}

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word16 -> Bool) -> ShortByteString -> [Int]
findIndices k sbs = go 0
  where
    l = length sbs
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n >= l    = []
          | k (w n)   = n : go (n + 1)
          | otherwise = go (n + 1)
{-# INLINE [1] findIndices #-}


-- --------------------------------------------------------------------
-- Internal

writeWord16Array :: MBA s -> Int -> Word16 -> ST s ()
writeWord16Array (MBA# mba#) (I# i#) (W16# w#) =
  ST $ \s -> case writeWord16Array# mba# i# w# s of
               s' -> (# s', () #)

indexWord16Array :: BA -> Int -> Word16
indexWord16Array (BA# ba#) (I# i#) = W16# (indexWord16Array# ba# i#)


packBytes :: [Word16] -> ShortByteString
packBytes cs = packLenBytes (List.length cs) cs

packLenBytes :: Int -> [Word16] -> ShortByteString
packLenBytes len ws0 =
    create (len * 2) (\mba -> go mba 0 ws0)
  where
    go :: MBA s -> Int -> [Word16] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord16Array mba i w
      go mba (i+1) ws

packBytesRev :: [Word16] -> ShortByteString
packBytesRev cs = packLenBytesRev (List.length cs) cs

packLenBytesRev :: Int -> [Word16] -> ShortByteString
packLenBytesRev len ws0 =
    create (len * 2) (\mba -> go mba len ws0)
  where
    go :: MBA s -> Int -> [Word16] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord16Array mba (i - 1) w
      go mba (i - 1) ws


unpackBytes :: ShortByteString -> [Word16]
unpackBytes bs = unpackAppendBytesLazy bs []

unpackAppendBytesLazy :: ShortByteString -> [Word16] -> [Word16]
unpackAppendBytesLazy sbs = go 0 (length sbs)
  where
    sz = 100

    go off len ws
      | len <= sz = unpackAppendBytesStrict sbs off len ws
      | otherwise = unpackAppendBytesStrict sbs off sz  remainder
                      where remainder = go (off+sz) (len-sz) ws


unpackAppendBytesStrict :: ShortByteString -> Int -> Int -> [Word16] -> [Word16]
unpackAppendBytesStrict !sbs off len = go (off-1) (off-1 + len)
  where
    go !sentinal !i !acc
      | i == sentinal = acc
      | otherwise     = let !w = indexWord16Array (asBA sbs) i
                         in go sentinal (i-1) (w:acc)


findIndexOrLength :: (Word16 -> Bool) -> ShortByteString -> Int
findIndexOrLength k sbs = go 0
  where
    l = length sbs
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n >= l     = l
          | k (w n)    = n
          | otherwise  = go (n + 1)
{-# INLINE findIndexOrLength #-}


findFromEndUntil :: (Word16 -> Bool) -> ShortByteString -> Int
findFromEndUntil k sbs = go (length sbs - 1)
  where
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n < 0     = 0
          | k (w n)   = n + 1
          | otherwise = go (n - 1)
{-# INLINE findFromEndUntil #-}


assertEven :: ShortByteString -> ShortByteString
assertEven sbs
  | even (BS.length sbs) = sbs
  | otherwise = error "Uneven number of bytes. This is not a Word16 bytestream."
