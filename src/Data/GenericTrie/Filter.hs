{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.GenericTrie.Filter
  ( SelectableTrieKey(..)
  , GSelectableTrieKey
  , genericSelect
  ) where

import Data.GenericTrie as Trie
import Data.Functor.Compose (Compose(..))
import GHC.Generics

class TrieKey k => SelectableTrieKey k where
  -- | Select a subtree of the 'Trie'. This is intended to support
  -- types with a "wildcard" key for doing more interesting filtering.
  select :: k -> Trie k a -> Trie k a

  default select ::
    ( GSelectableTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> Trie k a -> Trie k a
  select = genericSelect

defaultSelect :: TrieKey k => k -> Trie k a -> Trie k a
defaultSelect k = maybe empty (singleton k) . Trie.lookup k

instance SelectableTrieKey Int        where select = defaultSelect
instance SelectableTrieKey Integer    where select = defaultSelect
instance SelectableTrieKey Natural    where select = defaultSelect
instance SelectableTrieKey Char       where select = defaultSelect
instance (Show a, Ord a) => SelectableTrieKey (OrdKey a) where select = defaultSelect

instance SelectableTrieKey ()
instance SelectableTrieKey Bool
instance SelectableTrieKey a => SelectableTrieKey (Maybe a)
instance (SelectableTrieKey a, SelectableTrieKey b) => SelectableTrieKey (Either a b)
instance (SelectableTrieKey a, SelectableTrieKey b) => SelectableTrieKey (a,b)
instance (SelectableTrieKey a, SelectableTrieKey b, SelectableTrieKey c) => SelectableTrieKey (a,b,c)
instance SelectableTrieKey a => SelectableTrieKey [a]

-- | Generic implementation of 'select'. This is the default implementation.
genericSelect ::
    (GSelectableTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> Trie k a -> Trie k a
genericSelect k (MkTrie (Compose x)) = MkTrie (Compose (gselect (from k) =<< x))
{-# INLINABLE genericSelect #-}

class GTrieKey f => GSelectableTrieKey f where
  gselect                      :: f p -> GTrie f a -> Maybe (GTrie f a)

instance GSelectableTrieKey f => GSelectableTrieKey (M1 i c f) where
  gselect (M1 k) (MTrie x)      = fmap MTrie (gselect k x)

instance SelectableTrieKey a => GSelectableTrieKey (K1 i a) where
  gselect (K1 k) (KTrie x)      = fmap KTrie (checkNull (select k x))

instance (GSelectableTrieKey f, GSelectableTrieKey g) => GSelectableTrieKey (f :*: g) where
  gselect (i :*: j) (PTrie x)   = fmap PTrie (gmapMaybeWithKey (\_ -> gselect j) =<< gselect i x)

instance (GSelectableTrieKey f, GSelectableTrieKey g) => GSelectableTrieKey (f :+: g) where
  gselect (L1 k) (STrieL x)     = fmap STrieL (gselect k x)
  gselect (L1 k) (STrieB x _)   = fmap STrieL (gselect k x)
  gselect (R1 k) (STrieR y)     = fmap STrieR (gselect k y)
  gselect (R1 k) (STrieB _ y)   = fmap STrieR (gselect k y)
  gselect _      _              = Nothing

instance GSelectableTrieKey U1 where
  gselect _ x                   = Just x

instance GSelectableTrieKey V1 where
  gselect k t                   = k `seq` t `seq` error "GTrieKey.V1: gselect"

checkNull :: TrieKey k => Trie k a -> Maybe (Trie k a)
checkNull x
  | trieNull x = Nothing
  | otherwise  = Just x
