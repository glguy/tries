{-# LANGUAGE Safe #-}

{- |

This module implements an interface for working with "tries".
A key in the trie represents a distinct path through the trie.
This can provide benefits when using very large and possibly
very similar keys where comparing for order can become
expensive, and storing the various keys could be inefficient.

For primitive types like 'Int', this library will select efficient
implementations automatically.

All methods of 'TrieKey' can be derived automatically using
a 'Generic' instance.

@
data Demo = DemoC1 'Int' | DemoC2 'Int' 'Char'  deriving 'Generic'

instance 'TrieKey' Demo
@

-}

module Data.GenericTrie
  (
  -- * Trie interface
    TrieKey
  , Trie
  , alter
  , insert
  , insertWith
  , insertWith'
  , delete
  , empty
  , null
  , singleton
  , member
  , foldWithKey
  , fold
  , traverseWithKey
  , notMember
  , fromList
  , fromListWith
  , fromListWith'
  , toList
  , mapMaybe
  , mapMaybeWithKey
  , union
  , unionWith
  , unionWithKey
  , intersection
  , intersectionWith
  , intersectionWithKey
  , difference
  , differenceWith
  , differenceWithKey
  -- * Manual ord key instance selector
  , OrdKey(..)
  ) where

import Control.Applicative (Applicative)
import Data.List (foldl')
import Data.Maybe (isNothing, isJust)
import Prelude hiding (lookup, null)

import Data.GenericTrie.Internal

------------------------------------------------------------------------------
-- Various helpers
------------------------------------------------------------------------------

-- | Construct a trie from a list of key/value pairs
fromList :: TrieKey k => [(k,v)] -> Trie k v
fromList = foldl' (\acc (k,v) -> insert k v acc) empty

-- | Construct a trie from a list of key/value pairs.
-- The given function is used to combine values at the
-- same key.
fromListWith :: TrieKey k => (v -> v -> v) -> [(k,v)] -> Trie k v
fromListWith f = foldl' (\acc (k,v) -> insertWith f k v acc) empty

-- | Version of 'fromListWith' which is strict in the result of
-- the combining function.
fromListWith' :: TrieKey k => (v -> v -> v) -> [(k,v)] -> Trie k v
fromListWith' f = foldl' (\acc (k,v) -> insertWith' f k v acc) empty

-- | Construct an empty trie
empty :: TrieKey k => Trie k a
empty = trieEmpty
{-# INLINE empty #-}

-- | Test for an empty trie
null :: TrieKey k => Trie k a -> Bool
null = trieNull
{-# INLINE null #-}

-- | Lookup element from trie
lookup :: TrieKey k => k -> Trie k a -> Maybe a
lookup = trieLookup
{-# INLINE lookup #-}

-- | Insert element into trie
insert :: TrieKey k => k -> a -> Trie k a -> Trie k a
insert = trieInsert
{-# INLINE insert #-}

-- | Delete element from trie
delete :: TrieKey k => k -> Trie k a -> Trie k a
delete = trieDelete
{-# INLINE delete #-}

-- | Construct a trie holding a single value
singleton :: TrieKey k => k -> a -> Trie k a
singleton = trieSingleton
{-# INLINE singleton #-}

-- | Apply a function to the values of a 'Trie' and keep the elements
-- of the trie that result in a 'Just' value.
mapMaybeWithKey :: TrieKey k => (k -> a -> Maybe b) -> Trie k a -> Trie k b
mapMaybeWithKey = trieMapMaybeWithKey
{-# INLINE mapMaybeWithKey #-}

-- | Fold a trie with a function of both key and value.
fold :: TrieKey k => (a -> r -> r) -> r -> Trie k a -> r
fold = trieFoldWithKey . const
{-# INLINE fold #-}

-- | Fold a trie with a function of both key and value.
foldWithKey :: TrieKey k => (k -> a -> r -> r) -> r -> Trie k a -> r
foldWithKey = trieFoldWithKey
{-# INLINE foldWithKey #-}

-- | Traverse a trie with a function of both key and value.
traverseWithKey :: (TrieKey k, Applicative f) => (k -> a -> f b) -> Trie k a -> f (Trie k b)
traverseWithKey = trieTraverseWithKey
{-# INLINE traverseWithKey #-}

mergeWithKey ::
  TrieKey k =>
  (k -> a -> b -> Maybe c) ->
  (Trie k a -> Trie k c) ->
  (Trie k b -> Trie k c) ->
  Trie k a -> Trie k b -> Trie k c
mergeWithKey = trieMergeWithKey
{-# INLINE mergeWithKey #-}

-- | Alter the values of a trie. The function will take the value stored
-- as the given key if one exists and should return a value to insert at
-- that location or Nothing to delete from that location.
alter :: TrieKey k => k -> (Maybe a -> Maybe a) -> Trie k a -> Trie k a
alter k f t =
  case f (lookup k t) of
    Just v' -> insert k v' t
    Nothing -> delete k t

-- | Insert a value at the given key. The combining function is used
-- when a value is already stored at that key. The new value is the
-- first argument to the combining function.
insertWith :: TrieKey k => (v -> v -> v) -> k -> v -> Trie k v -> Trie k v
insertWith f k v = alter k $ \mb ->
                      case mb of
                        Just v0 -> Just (f v v0)
                        Nothing -> Just v

-- | Version of 'insertWith that is strict in the result of combining
-- two elements.
insertWith' :: TrieKey k => (v -> v -> v) -> k -> v -> Trie k v -> Trie k v
insertWith' f k v = alter k $ \mb ->
                      case mb of
                        Just v0 -> Just $! f v v0
                        Nothing -> Just v

-- | Returns 'True' when the 'Trie' has a value stored at the given key.
member :: TrieKey k => k -> Trie k a -> Bool
member k t = isJust (lookup k t)

-- | Returns 'False' when the 'Trie' has a value stored at the given key.
notMember :: TrieKey k => k -> Trie k a -> Bool
notMember k t = isNothing (lookup k t)

-- | Transform 'Trie' to an association list.
toList :: TrieKey k => Trie k a -> [(k,a)]
toList = foldWithKey (\k v xs -> (k,v) : xs) []

-- | Left-biased union of two tries
union :: TrieKey k => Trie k a -> Trie k a -> Trie k a
union = mergeWithKey (\_ a _ -> Just a) id id

-- | Union of two tries with function used to merge overlapping elements
unionWith :: TrieKey k => (a -> a -> a) -> Trie k a -> Trie k a -> Trie k a
unionWith f = mergeWithKey (\_ a b -> Just (f a b)) id id

-- | Union of two tries with function used to merge overlapping elements along with key
unionWithKey :: TrieKey k => (k -> a -> a -> a) -> Trie k a -> Trie k a -> Trie k a
unionWithKey f = mergeWithKey (\k a b -> Just (f k a b)) id id

-- | Left-biased intersection of two tries
intersection :: TrieKey k => Trie k a -> Trie k b -> Trie k a
intersection = mergeWithKey (\_ a _ -> Just a) (const empty) (const empty)

-- | Intersection of two tries parameterized by merge value merge function
intersectionWith :: TrieKey k => (a -> b -> c) -> Trie k a -> Trie k b -> Trie k c
intersectionWith f = mergeWithKey (\_ a b -> Just (f a b)) (const empty) (const empty)

-- | Intersection of two tries parameterized by merge value merge function with key
intersectionWithKey :: TrieKey k => (k -> a -> b -> c) -> Trie k a -> Trie k b -> Trie k c
intersectionWithKey f = mergeWithKey (\k a b -> Just (f k a b)) (const empty) (const empty)

-- | Remove keys from right trie from left trie
difference :: TrieKey k => Trie k a -> Trie k b -> Trie k a
difference = mergeWithKey (\_ _ _ -> Nothing) id (const empty)

-- | Parameterized 'difference' using a custom merge function. Return 'Just' to change
-- value stored in left trie, 'Nothing' to remove from left trie.
differenceWith :: TrieKey k => (a -> b -> Maybe a) -> Trie k a -> Trie k b -> Trie k a
differenceWith f = mergeWithKey (\_ -> f) id (const empty)

-- | 'differenceWithKey' where function also has access to the key
differenceWithKey :: TrieKey k => (k -> a -> b -> Maybe a) -> Trie k a -> Trie k b -> Trie k a
differenceWithKey f = mergeWithKey f id (const empty)

-- | Map a function over a trie filtering out elements where function returns 'Nothing'
mapMaybe :: TrieKey k => (a -> Maybe b) -> Trie k a -> Trie k b
mapMaybe f = mapMaybeWithKey (\_ -> f)
