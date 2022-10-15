{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-} -- coerce
{-# LANGUAGE CPP #-} -- MProxy on ghc >= 8
{-# LANGUAGE EmptyCase #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE DataKinds #-} -- Meta
#endif

-- | Unstable implementation details
module Data.GenericTrie.Internal
  ( TrieKey(..)
  , ShowTrieKey(..)
  , Trie(..)
  , OrdKey(..)
  , toList
  -- * Generic derivation implementation
  , genericTrieNull
  , genericTrieMap
  , genericTrieTraverse
  , genericTrieShowsPrec
  , genericInsert
  , genericLookup
  , genericDelete
  , genericAlter
  , genericMapMaybeWithKey
  , genericSingleton
  , genericEmpty
  , genericFoldWithKey
  , genericTraverseWithKey
  , genericTraverseMaybeWithKey
  , TrieRepDefault
  , GTrieKey(..)
  , GTrie(..)
  ) where

import Data.Char (chr, ord)
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
#if MIN_VERSION_base(4,9,0)
import Data.Kind (Type)
#endif
import Data.Map (Map)
import Data.Maybe (isNothing)
import GHC.Generics
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Prelude
#if MIN_VERSION_base(4,8,0)
import Data.Void (Void)
#endif
#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#endif

-- | Types that may be used as the key of a 'Trie'.
--
-- For @data@ declarations, the instance can be automatically derived from
-- a 'Generic' instance.
class TrieKey k where

  -- | Type of the representation of tries for this key.
#if MIN_VERSION_base(4,9,0)
  type TrieRep k :: Type -> Type
#else
  type TrieRep k :: * -> *
#endif

  -- | Construct an empty trie
  trieEmpty :: Trie k a

  -- | Test for an empty trie
  trieNull :: Trie k a -> Bool

  -- | Lookup element from trie
  trieLookup :: k -> Trie k a -> Maybe a

  -- | Insert element into trie
  trieInsert :: k -> a -> Trie k a -> Trie k a

  -- | Delete element from trie
  trieDelete :: k -> Trie k a -> Trie k a

  -- | Insert, modify, or remove an element in a trie
  trieAlter :: k -> (Maybe a -> Maybe a) -> Trie k a -> Trie k a

  -- | Construct a trie holding a single value
  trieSingleton :: k -> a -> Trie k a

  -- | Apply a function to all values stored in a trie
  trieMap :: (a -> b) -> Trie k a -> Trie k b

  -- | Traverse the values stored in a trie
  trieTraverse :: Applicative f => (a -> f b) -> Trie k a -> f (Trie k b)

  -- | Apply a function to the values of a 'Trie' and keep the elements
  -- of the trie that result in a 'Just' value.
  trieMapMaybeWithKey :: (k -> a -> Maybe b) -> Trie k a -> Trie k b

  -- | Fold a trie with a function of both key and value.
  trieFoldWithKey :: (k -> a -> r -> r) -> r -> Trie k a -> r

  -- | Traverse a trie with a function of both key and value.
  trieTraverseWithKey :: Applicative f => (k -> a -> f b) -> Trie k a -> f (Trie k b)

  -- | Traverse a trie with a function of both key and value, and keep the elements
  -- of the trie that result in a 'Just' value.
  trieTraverseMaybeWithKey :: Applicative f => (k -> a -> f (Maybe b)) -> Trie k a -> f (Trie k b)

  trieMergeWithKey :: (k -> a -> b -> Maybe c) ->
                      (Trie k a -> Trie k c) ->
                      (Trie k b -> Trie k c) ->
                      Trie k a -> Trie k b -> Trie k c


  -- Defaults using 'Generic'

  type instance TrieRep k = TrieRepDefault k

  default trieEmpty :: ( TrieRep k ~ TrieRepDefault k) => Trie k a
  trieEmpty = genericEmpty

  default trieSingleton ::
    ( GTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> a -> Trie k a
  trieSingleton = genericSingleton

  default trieNull ::
    ( TrieRep k ~ TrieRepDefault k) =>
    Trie k a -> Bool
  trieNull = genericTrieNull

  default trieLookup ::
    ( GTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> Trie k a -> Maybe a
  trieLookup = genericLookup

  default trieInsert ::
    ( GTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> a -> Trie k a -> Trie k a
  trieInsert = genericInsert

  default trieDelete ::
    ( GTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> Trie k a -> Trie k a
  trieDelete = genericDelete

  default trieAlter ::
    ( GTrieKey (Rep k), Generic k , TrieRep k ~ TrieRepDefault k) =>
    k -> (Maybe a -> Maybe a) -> Trie k a -> Trie k a
  trieAlter = genericAlter

  default trieMap ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k) =>
    (a -> b) -> Trie k a -> Trie k b
  trieMap = genericTrieMap

  default trieTraverse ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k , Applicative f) =>
    (a -> f b) -> Trie k a -> f (Trie k b)
  trieTraverse = genericTrieTraverse

  default trieMapMaybeWithKey ::
    ( GTrieKey (Rep k) , Generic k, TrieRep k ~ TrieRepDefault k) =>
    (k -> a -> Maybe b) -> Trie k a -> Trie k b
  trieMapMaybeWithKey = genericMapMaybeWithKey

  default trieFoldWithKey ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k, Generic k) =>
    (k -> a -> r -> r) -> r -> Trie k a -> r
  trieFoldWithKey = genericFoldWithKey

  default trieTraverseWithKey ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k, Generic k, Applicative f) =>
    (k -> a -> f b) -> Trie k a -> f (Trie k b)
  trieTraverseWithKey = genericTraverseWithKey

  default trieTraverseMaybeWithKey ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k, Generic k, Applicative f) =>
    (k -> a -> f (Maybe b)) -> Trie k a -> f (Trie k b)
  trieTraverseMaybeWithKey = genericTraverseMaybeWithKey

  default trieMergeWithKey ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k, Generic k ) =>
    (k -> a -> b -> Maybe c) ->
    (Trie k a -> Trie k c) ->
    (Trie k b -> Trie k c) ->
    Trie k a -> Trie k b -> Trie k c
  trieMergeWithKey = genericMergeWithKey

-- | A map from keys of type @k@, to values of type @a@.
newtype Trie k a = MkTrie (TrieRep k a)

-- | Transform a trie to an association list.
toList :: TrieKey k => Trie k a -> [(k,a)]
toList = trieFoldWithKey (\k v xs -> (k,v) : xs) []

class TrieKey k => ShowTrieKey k where
  -- | Show a representation of the internal structure of a trie
  trieShowsPrec :: Show a => Int -> Trie k a -> ShowS
  default trieShowsPrec ::
    ( Show a, GTrieKeyShow (Rep k) , TrieRep k ~ TrieRepDefault k) =>
    Int -> Trie k a -> ShowS
  trieShowsPrec = genericTrieShowsPrec


------------------------------------------------------------------------------
-- Manually derived instances for base types
------------------------------------------------------------------------------

-- | 'Int' tries are implemented with 'IntMap'.
instance TrieKey Int where
  type TrieRep Int              = IntMap
  trieLookup k (MkTrie x)       = IntMap.lookup k x
  trieInsert k v (MkTrie t)     = MkTrie (IntMap.insert k v t)
  trieDelete k (MkTrie t)       = MkTrie (IntMap.delete k t)
  trieAlter k f (MkTrie t)      = MkTrie (IntMap.alter f k t)
  trieEmpty                     = MkTrie IntMap.empty
  trieSingleton k v             = MkTrie (IntMap.singleton k v)
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey f x)
  trieTraverseMaybeWithKey f (MkTrie x) =
#if MIN_VERSION_containers (0,6,4)
    MkTrie <$> IntMap.traverseMaybeWithKey f x
#else
    MkTrie . IntMap.mapMaybe id <$> IntMap.traverseWithKey f x
#endif
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldrWithKey f z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey f x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey f (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieAlter #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}
  {-# INLINABLE trieTraverseMaybeWithKey #-}

instance ShowTrieKey Int where
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINABLE trieShowsPrec #-}

-- | 'Integer' tries are implemented with 'Map'.
instance TrieKey Integer where
  type TrieRep Integer              = Map Integer
  trieLookup k (MkTrie t)           = Map.lookup k t
  trieInsert k v (MkTrie t)         = MkTrie (Map.insert k v t)
  trieDelete k (MkTrie t)           = MkTrie (Map.delete k t)
  trieAlter k f (MkTrie t)          = MkTrie (Map.alter f k t)
  trieEmpty                         = MkTrie Map.empty
  trieSingleton k v                 = MkTrie (Map.singleton k v)
  trieNull (MkTrie x)               = Map.null x
  trieMap f (MkTrie x)              = MkTrie (Map.map f x)
  trieTraverse f (MkTrie x)         = fmap MkTrie (traverse f x)
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (Map.mapMaybeWithKey f x)
  trieTraverseMaybeWithKey f (MkTrie x) =
#if MIN_VERSION_containers (0,5,8)
    MkTrie <$> Map.traverseMaybeWithKey f x
#else
    MkTrie . Map.mapMaybe id <$> Map.traverseWithKey f x
#endif
  trieFoldWithKey f z (MkTrie x)    = Map.foldrWithKey f z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (Map.traverseWithKey f x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (Map.mergeWithKey f (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieAlter #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieTraverseMaybeWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

instance ShowTrieKey Integer where
  trieShowsPrec p (MkTrie x)        = showsPrec p x
  {-# INLINABLE trieShowsPrec #-}

#if MIN_VERSION_base(4,8,0)
-- | 'Natural' tries are implemented with 'Map'.
instance TrieKey Natural where
  type TrieRep Natural              = Map Natural
  trieLookup k (MkTrie t)           = Map.lookup k t
  trieInsert k v (MkTrie t)         = MkTrie (Map.insert k v t)
  trieDelete k (MkTrie t)           = MkTrie (Map.delete k t)
  trieAlter k f (MkTrie t)          = MkTrie (Map.alter f k t)
  trieEmpty                         = MkTrie Map.empty
  trieSingleton k v                 = MkTrie (Map.singleton k v)
  trieNull (MkTrie x)               = Map.null x
  trieMap f (MkTrie x)              = MkTrie (Map.map f x)
  trieTraverse f (MkTrie x)         = fmap MkTrie (traverse f x)
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (Map.mapMaybeWithKey f x)
  trieTraverseMaybeWithKey f (MkTrie x) =
#if MIN_VERSION_containers (0,5,8)
    MkTrie <$> Map.traverseMaybeWithKey f x
#else
    MkTrie . Map.mapMaybe id <$> Map.traverseWithKey f x
#endif
  trieFoldWithKey f z (MkTrie x)    = Map.foldrWithKey f z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (Map.traverseWithKey f x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (Map.mergeWithKey f (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieAlter #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieTraverseMaybeWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

instance ShowTrieKey Natural where
  trieShowsPrec p (MkTrie x)        = showsPrec p x
  {-# INLINABLE trieShowsPrec #-}
#endif

-- | 'Word' tries are implemented with 'IntMap'.
instance TrieKey Word where
  type TrieRep Word                 = IntMap
  trieLookup k (MkTrie t)           = IntMap.lookup (fromIntegral k) t
  trieDelete k (MkTrie t)           = MkTrie (IntMap.delete (fromIntegral k) t)
  trieInsert k v (MkTrie t)         = MkTrie (IntMap.insert (fromIntegral k) v t)
  trieAlter k f (MkTrie t)          = MkTrie (IntMap.alter f (fromIntegral k) t)
  trieEmpty                         = MkTrie IntMap.empty
  trieSingleton k v                 = MkTrie (IntMap.singleton (fromIntegral k) v)
  trieNull (MkTrie x)               = IntMap.null x
  trieMap f (MkTrie x)              = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)         = fmap MkTrie (traverse f x)
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey (f . fromIntegral) x)
  trieTraverseMaybeWithKey f (MkTrie x) =
#if MIN_VERSION_containers (0,6,4)
    MkTrie <$> IntMap.traverseMaybeWithKey (f . fromIntegral) x
#else
    MkTrie . IntMap.mapMaybe id <$> IntMap.traverseWithKey (f . fromIntegral) x
#endif
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldrWithKey (f . fromIntegral) z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey (f . fromIntegral) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey (f . fromIntegral) (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieAlter #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieTraverseMaybeWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

instance ShowTrieKey Word where
  trieShowsPrec p (MkTrie x) =
    showParen (p > 10) (showString "fromList " . shows [(fromIntegral k :: Word, v) | (k,v) <- IntMap.toList x])
  {-# INLINABLE trieShowsPrec #-}

-- | 'Char' tries are implemented with 'IntMap'.
instance TrieKey Char where
  type TrieRep Char                 = IntMap
  trieLookup k (MkTrie t)           = IntMap.lookup (ord k) t
  trieDelete k (MkTrie t)           = MkTrie (IntMap.delete (ord k) t)
  trieInsert k v (MkTrie t)         = MkTrie (IntMap.insert (ord k) v t)
  trieAlter k f (MkTrie t)          = MkTrie (IntMap.alter f (ord k) t)
  trieEmpty                         = MkTrie IntMap.empty
  trieSingleton k v                 = MkTrie (IntMap.singleton (ord k) v)
  trieNull (MkTrie x)               = IntMap.null x
  trieMap f (MkTrie x)              = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)         = fmap MkTrie (traverse f x)
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey (f . chr) x)
  trieTraverseMaybeWithKey f (MkTrie x) =
#if MIN_VERSION_containers (0,6,4)
    MkTrie <$> IntMap.traverseMaybeWithKey (f . chr) x
#else
    MkTrie . IntMap.mapMaybe id <$> IntMap.traverseWithKey (f . chr) x
#endif
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldrWithKey (f . chr) z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey (f . chr) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey (f . chr) (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieAlter #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieTraverseMaybeWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

instance ShowTrieKey Char where
  trieShowsPrec p (MkTrie x)        = showsPrec p x
  {-# INLINABLE trieShowsPrec #-}

-- | Tries indexed by 'OrdKey' will be represented as an ordinary 'Map'
-- and the keys will be compared based on the 'Ord' instance for @k@.
newtype OrdKey k = OrdKey { getOrdKey :: k }
  deriving (Read, Show, Eq, Ord)

-- | 'OrdKey' tries are implemented with 'Map', this is
-- intended for cases where it is better for some reason
-- to force the use of a 'Map' than to use the generically
-- derived structure.
instance Ord k => TrieKey (OrdKey k) where
  type TrieRep (OrdKey k)               = Map k
  trieLookup (OrdKey k) (MkTrie x)      = Map.lookup k x
  trieInsert (OrdKey k) v (MkTrie x)    = MkTrie (Map.insert k v x)
  trieDelete (OrdKey k) (MkTrie x)      = MkTrie (Map.delete k x)
  trieAlter (OrdKey k) f (MkTrie x)     = MkTrie (Map.alter f k x)
  trieEmpty                             = MkTrie Map.empty
  trieSingleton (OrdKey k) v            = MkTrie (Map.singleton k v)
  trieNull (MkTrie x)                   = Map.null x
  trieMap f (MkTrie x)                  = MkTrie (Map.map f x)
  trieTraverse f (MkTrie x)             = fmap MkTrie (traverse f x)
  trieMapMaybeWithKey f (MkTrie x)      = MkTrie (Map.mapMaybeWithKey (f . OrdKey) x)
  trieTraverseMaybeWithKey f (MkTrie x) =
#if MIN_VERSION_containers (0,5,8)
    MkTrie <$> Map.traverseMaybeWithKey (f . OrdKey) x
#else
    MkTrie . Map.mapMaybe id <$> Map.traverseWithKey (f . OrdKey) x
#endif
  trieFoldWithKey f z (MkTrie x)        = Map.foldrWithKey (f . OrdKey) z x
  trieTraverseWithKey f (MkTrie x)      = fmap MkTrie (Map.traverseWithKey (f . OrdKey) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (Map.mergeWithKey (f . OrdKey) (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieAlter #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieTraverseMaybeWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

instance (Show k, Ord k) => ShowTrieKey (OrdKey k) where
  trieShowsPrec p (MkTrie x)            = showsPrec p x
  {-# INLINABLE trieShowsPrec #-}

------------------------------------------------------------------------------
-- Automatically derived instances for common types
------------------------------------------------------------------------------

#if MIN_VERSION_base(4,8,0)
instance                                      TrieKey Void
instance                                      ShowTrieKey Void
#endif
instance                                      TrieKey ()
instance                                      ShowTrieKey ()
instance                                      TrieKey Bool
instance                                      ShowTrieKey Bool
instance                                      TrieKey Ordering
instance                                      ShowTrieKey Ordering
instance TrieKey k                         => TrieKey (Maybe k)
instance ShowTrieKey k                     => ShowTrieKey (Maybe k)
instance (TrieKey a, TrieKey b)            => TrieKey (Either a b)
instance (ShowTrieKey a, ShowTrieKey b)    => ShowTrieKey (Either a b)
instance (TrieKey a, TrieKey b)            => TrieKey (a,b)
instance (ShowTrieKey a, ShowTrieKey b)    => ShowTrieKey (a,b)
instance (TrieKey a, TrieKey b, TrieKey c) => TrieKey (a,b,c)
instance (ShowTrieKey a, ShowTrieKey b, ShowTrieKey c) => ShowTrieKey (a,b,c)
instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d) => TrieKey (a,b,c,d)
instance (ShowTrieKey a, ShowTrieKey b, ShowTrieKey c, ShowTrieKey d) => ShowTrieKey (a,b,c,d)
instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d, TrieKey e) => TrieKey (a,b,c,d,e)
instance (ShowTrieKey a, ShowTrieKey b, ShowTrieKey c, ShowTrieKey d, ShowTrieKey e) => ShowTrieKey (a,b,c,d,e)
instance TrieKey k                         => TrieKey [k]
instance ShowTrieKey k                     => ShowTrieKey [k]

------------------------------------------------------------------------------
-- Generic 'TrieKey' method implementations
------------------------------------------------------------------------------

-- | Generic implementation of 'lookup'. This is the default implementation.
genericLookup ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> Trie k a -> Maybe a
genericLookup k t = gtrieLookup (from k) =<< unwrap t
{-# INLINABLE genericLookup #-}

-- | Generic implementation of 'trieNull'. This is the default implementation.
genericTrieNull ::
    ( TrieRep k ~ TrieRepDefault k
    ) =>
    Trie k a -> Bool
genericTrieNull = isNothing . unwrap
{-# INLINABLE genericTrieNull #-}

-- | Generic implementation of 'singleton'. This is the default implementation.
genericSingleton ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> a -> Trie k a
genericSingleton k v = wrap $ Just $! gtrieSingleton (from k) v
{-# INLINABLE genericSingleton #-}

-- | Generic implementation of 'empty'. This is the default implementation.
genericEmpty ::
    ( TrieRep k ~ TrieRepDefault k
    ) =>
    Trie k a
genericEmpty = MkTrie EmptyTrie
{-# INLINABLE genericEmpty #-}

-- | Generic implementation of 'insert'. This is the default implementation.
genericInsert ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> a -> Trie k a -> Trie k a
genericInsert k v m = wrap $
  case unwrap m of
    Nothing -> Just $! gtrieSingleton (from k) v
    Just t  -> Just $! gtrieInsert    (from k) v t
{-# INLINABLE genericInsert #-}

-- | Generic implementation of 'delete'. This is the default implementation.
genericDelete ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> Trie k a -> Trie k a
genericDelete k m = wrap (gtrieDelete (from k) =<< unwrap m)
{-# INLINABLE genericDelete #-}

-- | Generic implementation of 'alter'. This is the default implementation.
genericAlter ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> (Maybe a -> Maybe a) -> Trie k a -> Trie k a
genericAlter k f m = wrap (gtrieAlter (from k) f =<< unwrap m)
{-# INLINABLE genericAlter #-}

-- | Generic implementation of 'trieMap'. This is the default implementation.
genericTrieMap ::
    ( GTrieKey (Rep k)
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (a -> b) -> Trie k a -> Trie k b
genericTrieMap f x = wrap (fmap (gtrieMap f) $! unwrap x)
{-# INLINABLE genericTrieMap #-}


-- | Generic implementation of 'trieTraverse'. This is the default implementation.
genericTrieTraverse ::
    ( GTrieKey (Rep k)
    , TrieRep k ~ TrieRepDefault k
    , Applicative f
    ) =>
    (a -> f b) -> Trie k a -> f (Trie k b)
genericTrieTraverse f x =
  fmap wrap (traverse (gtrieTraverse f) (unwrap x))
{-# INLINABLE genericTrieTraverse #-}

-- | Generic implementation of 'trieShowsPrec'. This is the default implementation.
genericTrieShowsPrec ::
    ( Show a, GTrieKeyShow (Rep k)
    , TrieRep k ~ TrieRepDefault k
    ) =>
    Int -> Trie k a -> ShowS
genericTrieShowsPrec p m =
  case unwrap m of
    Just x  -> showsPrec p x
    Nothing -> showString "()"
{-# INLINABLE genericTrieShowsPrec #-}

-- | Generic implementation of 'mapMaybe'. This is the default implementation.
genericMapMaybeWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (k -> a -> Maybe b) -> Trie k a -> Trie k b
genericMapMaybeWithKey f x = wrap (gmapMaybeWithKey (f . to) =<< unwrap x)
{-# INLINABLE genericMapMaybeWithKey #-}

-- | Generic implementation of 'foldWithKey'. This is the default implementation.
genericFoldWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (k -> a -> r -> r) -> r -> Trie k a -> r
genericFoldWithKey f z m =
  case unwrap m of
    Nothing -> z
    Just x  -> gfoldWithKey (f . to) z x
{-# INLINABLE genericFoldWithKey #-}

-- | Generic implementation of 'traverseWithKey'. This is the default implementation.
genericTraverseWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    , Applicative f
    ) =>
    (k -> a -> f b) -> Trie k a -> f (Trie k b)
genericTraverseWithKey f m = fmap wrap (traverse (gtraverseWithKey (f . to)) (unwrap m))
{-# INLINABLE genericTraverseWithKey #-}

-- | Generic implementation of 'traverseMaybeWithKey'. This is the default implementation.
genericTraverseMaybeWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    , Applicative f
    ) =>
    (k -> a -> f (Maybe b)) -> Trie k a -> f (Trie k b)
genericTraverseMaybeWithKey f m = fmap (maybe (MkTrie EmptyTrie) wrap) (traverse (gtraverseMaybeWithKey (f . to)) (unwrap m))
{-# INLINABLE genericTraverseMaybeWithKey #-}

-- | Generic implementation of 'mergeWithKey'. This is the default implementation.
genericMergeWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (k -> a -> b -> Maybe c) -> (Trie k a -> Trie k c) -> (Trie k b -> Trie k c) ->
    Trie k a -> Trie k b -> Trie k c
genericMergeWithKey f g h (MkTrie x) (MkTrie y) =
  case (x,y) of
    (EmptyTrie, EmptyTrie) -> MkTrie EmptyTrie
    (NonEmptyTrie{} , EmptyTrie) -> g (MkTrie x)
    (EmptyTrie, NonEmptyTrie{} ) -> h (MkTrie y)
    (NonEmptyTrie x', NonEmptyTrie y') -> wrap (gmergeWithKey (f . to) (aux g) (aux h) x' y')
      where
      aux k t = unwrap (k (MkTrie (NonEmptyTrie t)))
{-# INLINABLE genericMergeWithKey #-}

wrap :: TrieRep k ~ TrieRepDefault k1 => Maybe (GTrie (Rep k1) a) -> Trie k a
wrap Nothing = MkTrie EmptyTrie
wrap (Just t) = MkTrie (NonEmptyTrie t)

unwrap :: TrieRep t ~ TrieRepDefault t2 => Trie t t1 -> Maybe (GTrie (Rep t2) t1)
unwrap (MkTrie EmptyTrie) = Nothing
unwrap (MkTrie (NonEmptyTrie t)) = Just t


------------------------------------------------------------------------------
-- Generic implementation class
------------------------------------------------------------------------------

-- | The default implementation of a 'TrieRep' is 'GTrie' wrapped in
-- a 'Maybe'. This wrapping is due to the 'GTrie' being a non-empty
-- trie allowing all the of the "emptiness" to be represented at the
-- top level for any given generically implemented key.
data TrieRepDefault k a = EmptyTrie | NonEmptyTrie !(GTrie (Rep k) a)

-- | Mapping of generic representation of keys to trie structures.
#if __GLASGOW_HASKELL__ >= 810
type GTrie :: (Type -> Type) -> Type -> Type
data    family   GTrie f a
#elif MIN_VERSION_base(4,9,0)
data    family   GTrie (f :: Type -> Type) a
#else
data    family   GTrie (f :: * -> *) a
#endif

newtype instance GTrie (M1 i c f) a     = MTrie (GTrie f a)
data    instance GTrie (f :+: g)  a     = STrieL !(GTrie f a)
                                        | STrieR !(GTrie g a)
                                        | STrieB !(GTrie f a) !(GTrie g a)
newtype instance GTrie (f :*: g)  a     = PTrie (GTrie f (GTrie g a))
newtype instance GTrie (K1 i k)   a     = KTrie (Trie k a)
newtype instance GTrie U1         a     = UTrie a
data    instance GTrie V1         a

instance GTrieKey f => Functor (GTrie f) where
  fmap = gtrieMap

-- | TrieKey operations on Generic representations used to provide
-- the default implementations of tries.
class GTrieKey f where
  gtrieLookup    :: f p -> GTrie f a -> Maybe a
  gtrieInsert    :: f p -> a -> GTrie f a -> GTrie f a
  gtrieSingleton :: f p -> a -> GTrie f a
  gtrieDelete    :: f p -> GTrie f a -> Maybe (GTrie f a)
  gtrieAlter     :: f p -> (Maybe a -> Maybe a) -> GTrie f a -> Maybe (GTrie f a)
  gtrieMap       :: (a -> b) -> GTrie f a -> GTrie f b
  gtrieTraverse  :: Applicative m => (a -> m b) -> GTrie f a -> m (GTrie f b)
  gmapMaybeWithKey :: (f p -> a -> Maybe b) -> GTrie f a -> Maybe (GTrie f b)
  gfoldWithKey   :: (f p -> a -> r -> r) -> r -> GTrie f a -> r
  gtraverseWithKey :: Applicative m => (f p -> a -> m b) -> GTrie f a -> m (GTrie f b)
  gtraverseMaybeWithKey :: Applicative m => (f p -> a -> m (Maybe b)) -> GTrie f a -> m (Maybe (GTrie f b))
  gmergeWithKey  :: (f p -> a -> b -> Maybe c) ->
                    (GTrie f a -> Maybe (GTrie f c)) ->
                    (GTrie f b -> Maybe (GTrie f c)) ->
                    GTrie f a -> GTrie f b -> Maybe (GTrie f c)

-- | The 'GTrieKeyShow' class provides generic implementations
-- of 'showsPrec'. This class is separate due to its implementation
-- varying for different kinds of metadata.
class GTrieKeyShow f where
  gtrieShowsPrec :: Show a => Int -> GTrie f a -> ShowS

------------------------------------------------------------------------------
-- Generic implementation for metadata
------------------------------------------------------------------------------

-- | Generic metadata is skipped in trie representation and operations.
instance GTrieKey f => GTrieKey (M1 i c f) where
  gtrieLookup (M1 k) (MTrie x)  = gtrieLookup k x
  gtrieInsert (M1 k) v (MTrie t)= MTrie (gtrieInsert k v t)
  gtrieSingleton (M1 k) v       = MTrie (gtrieSingleton k v)
  gtrieDelete (M1 k) (MTrie x)  = fmap MTrie (gtrieDelete k x)
  gtrieAlter (M1 k) f (MTrie x) = fmap MTrie (gtrieAlter k f x)
  gtrieMap f (MTrie x)          = MTrie (gtrieMap f x)
  gtrieTraverse f (MTrie x)     = fmap MTrie (gtrieTraverse f x)
  gmapMaybeWithKey f (MTrie x)  = fmap MTrie (gmapMaybeWithKey (f . M1) x)
  gfoldWithKey f z (MTrie x)    = gfoldWithKey (f . M1) z x
  gtraverseWithKey f (MTrie x)  = fmap MTrie (gtraverseWithKey (f . M1) x)
  gtraverseMaybeWithKey f (MTrie x)  = fmap coerce (gtraverseMaybeWithKey (f . M1) x)
  gmergeWithKey f g h (MTrie x) (MTrie y) = fmap MTrie (gmergeWithKey (f . M1) (coerce g) (coerce h) x y)
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtraverseMaybeWithKey #-}

#if MIN_VERSION_base(4,9,0)
data MProxy (c :: Meta) (f :: Type -> Type) a = MProxy
#else
data MProxy (c :: *)    (f :: * -> *) a = MProxy
#endif

instance GTrieKeyShow f => GTrieKeyShow (M1 D d f) where
  gtrieShowsPrec p (MTrie x)    = showsPrec p x
instance (Constructor c, GTrieKeyShow f) => GTrieKeyShow (M1 C c f) where
  gtrieShowsPrec p (MTrie x)    = showParen (p > 10)
                                $ showString "Con "
                                . shows (conName (MProxy :: MProxy c f ()))
                                . showString " "
                                . showsPrec 11 x
instance GTrieKeyShow f => GTrieKeyShow (M1 S s f) where
  gtrieShowsPrec p (MTrie x)    = showsPrec p x

------------------------------------------------------------------------------
-- Generic implementation for fields
------------------------------------------------------------------------------

checkNull :: TrieKey k => Trie k a -> Maybe (Trie k a)
checkNull x
  | trieNull x = Nothing
  | otherwise  = Just x

-- | Generic fields are represented by tries of the field type.
instance TrieKey k => GTrieKey (K1 i k) where
  gtrieLookup (K1 k) (KTrie x)          = trieLookup k x
  gtrieInsert (K1 k) v (KTrie t)        = KTrie (trieInsert k v t)
  gtrieSingleton (K1 k) v               = KTrie (trieSingleton k v)
  gtrieDelete (K1 k) (KTrie t)          = fmap KTrie (checkNull (trieDelete k t))
  gtrieAlter (K1 k) f (KTrie t)         = fmap KTrie (checkNull (trieAlter k f t))
  gtrieMap f (KTrie x)                  = KTrie (trieMap f x)
  gtrieTraverse f (KTrie x)             = fmap KTrie (trieTraverse f x)
  gmapMaybeWithKey f (KTrie x)          = fmap KTrie (checkNull (trieMapMaybeWithKey (f . K1) x))
  gfoldWithKey f z (KTrie x)            = trieFoldWithKey (f . K1) z x
  gtraverseWithKey f (KTrie x)          = fmap KTrie (trieTraverseWithKey (f . K1) x)
  gtraverseMaybeWithKey f (KTrie x)     = fmap (fmap KTrie . checkNull) (trieTraverseMaybeWithKey (f . K1) x)
  gmergeWithKey f g h (KTrie x) (KTrie y) = fmap KTrie (checkNull (trieMergeWithKey (f . K1) g' h' x y))
     where
     g' t = case g (KTrie t) of
              Just (KTrie t') -> t'
              Nothing         -> trieEmpty
     h' t = case h (KTrie t) of
              Just (KTrie t') -> t'
              Nothing         -> trieEmpty
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtraverseMaybeWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}

instance ShowTrieKey k => GTrieKeyShow (K1 i k) where
  gtrieShowsPrec p (KTrie x)            = trieShowsPrec p x

------------------------------------------------------------------------------
-- Generic implementation for products
------------------------------------------------------------------------------

-- | Generic products are represented by tries of tries.
instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where

  gtrieLookup (i :*: j) (PTrie x)       = gtrieLookup j =<< gtrieLookup i x
  gtrieInsert (i :*: j) v (PTrie t)     = case gtrieAlter i f t of
  -- The "impossible" error is unfortunate. We *could* add another function
  --
  --   ginsertChange :: f i -> (Maybe a -> a) -> GTrie i a -> GTrie i a
  --
  -- and use that, but that would only push the "impossible" errors to the
  -- implementations using `Map` and `IntMap`, which doesn't seem to help.
                                            Nothing -> error "gtrieInsert: insertion produced an empty trie"
                                            Just t' -> PTrie t'
    where
      -- f :: Maybe (GTrie g a) -> Maybe (GTrie g a)
      f Nothing = Just $! gtrieSingleton j v
      f (Just u) = Just $! gtrieInsert j v u

  gtrieDelete (i :*: j) (PTrie t)       = fmap PTrie (gtrieAlter i f t)
    where
      -- f :: Maybe (GTrie g a) -> Maybe (GTrie g a)
      f Nothing = Nothing
      f (Just u) = gtrieDelete j u

  gtrieAlter (i :*: j) f (PTrie t)      = fmap PTrie (gtrieAlter i g t)
    where
      -- g :: Maybe (GTrie g a) -> Maybe (GTrie g a)
      g Nothing =
        case f Nothing of
          Nothing -> Nothing
          Just v -> Just $! gtrieSingleton j v
      g (Just u) = gtrieAlter j f u

  gtrieSingleton (i :*: j) v            = PTrie (gtrieSingleton i (gtrieSingleton j v))
  gtrieMap f (PTrie x)                  = PTrie (gtrieMap (gtrieMap f) x)
  gtrieTraverse f (PTrie x)             = fmap PTrie (gtrieTraverse (gtrieTraverse f) x)
  gmapMaybeWithKey f (PTrie x)          = fmap PTrie (gmapMaybeWithKey (\i -> gmapMaybeWithKey (\j -> f (i:*:j))) x)
  gfoldWithKey f z (PTrie x)            = gfoldWithKey (\i m r -> gfoldWithKey (\j -> f (i:*:j)) r m) z x
  gtraverseWithKey f (PTrie x)          = fmap PTrie (gtraverseWithKey (\i ->
                                                      gtraverseWithKey (\j -> f (i :*: j))) x)
  gtraverseMaybeWithKey f (PTrie x)     = fmap (fmap PTrie) (gtraverseMaybeWithKey (\i ->
                                                      gtraverseMaybeWithKey (\j -> f (i :*: j))) x)
  gmergeWithKey f g h (PTrie x) (PTrie y) =
    fmap PTrie $!
       gmergeWithKey
         (\i ->
           gmergeWithKey
             (\j -> f (i:*:j))
             (g' i)
             (h' i))
         (coerce g)
         (coerce h)
         x
         y
    where
    g' i t = do PTrie t' <- g (PTrie (gtrieSingleton i t))
                gtrieLookup i t'
    h' i t = do PTrie t' <- h (PTrie (gtrieSingleton i t))
                gtrieLookup i t'

  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtraverseMaybeWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}

instance (GTrieKeyShow f, GTrieKeyShow g) => GTrieKeyShow (f :*: g) where
  gtrieShowsPrec p (PTrie x)            = showsPrec p x


------------------------------------------------------------------------------
-- Generic implementation for sums
------------------------------------------------------------------------------

-- | Generic sums are represented by up to a pair of sub-tries.
instance (GTrieKey f, GTrieKey g) => GTrieKey (f :+: g) where

  gtrieLookup (L1 k) (STrieL x)         = gtrieLookup k x
  gtrieLookup (L1 k) (STrieB x _)       = gtrieLookup k x
  gtrieLookup (R1 k) (STrieR y)         = gtrieLookup k y
  gtrieLookup (R1 k) (STrieB _ y)       = gtrieLookup k y
  gtrieLookup _      _                  = Nothing

  gtrieInsert (L1 k) v (STrieL x)       = STrieL (gtrieInsert k v x)
  gtrieInsert (L1 k) v (STrieR y)       = STrieB (gtrieSingleton k v) y
  gtrieInsert (L1 k) v (STrieB x y)     = STrieB (gtrieInsert k v x) y
  gtrieInsert (R1 k) v (STrieL x)       = STrieB x (gtrieSingleton k v)
  gtrieInsert (R1 k) v (STrieR y)       = STrieR (gtrieInsert k v y)
  gtrieInsert (R1 k) v (STrieB x y)     = STrieB x (gtrieInsert k v y)

  gtrieSingleton (L1 k) v               = STrieL (gtrieSingleton k v)
  gtrieSingleton (R1 k) v               = STrieR (gtrieSingleton k v)

  gtrieDelete (L1 k) (STrieL x)         = fmap STrieL $! gtrieDelete k x
  gtrieDelete (L1 _) (STrieR y)         = Just $! STrieR y
  gtrieDelete (L1 k) (STrieB x y)       = case gtrieDelete k x of
                                            Nothing -> Just $! STrieR y
                                            Just x' -> Just $! STrieB x' y
  gtrieDelete (R1 _) (STrieL x)         = Just $! STrieL x
  gtrieDelete (R1 k) (STrieR y)         = fmap STrieR $! gtrieDelete k y
  gtrieDelete (R1 k) (STrieB x y)       = case gtrieDelete k y of
                                            Nothing -> Just $! STrieL x
                                            Just y' -> Just $! STrieB x y'

  gtrieAlter (L1 k) f (STrieL x)        = do
                                            x' <- gtrieAlter k f x
                                            Just $! STrieL x'
  gtrieAlter (L1 k) f t@(STrieR y)      = case f Nothing of
                                            Nothing -> Just t
                                            Just v -> Just $! STrieB (gtrieSingleton k v) y
  gtrieAlter (L1 k) f (STrieB x y)      = case gtrieAlter k f x of
                                            Just x' -> Just $! STrieB x' y
                                            Nothing -> Just $! STrieR y
  gtrieAlter (R1 k) f (STrieR y)        = do
                                            y' <- gtrieAlter k f y
                                            Just $! STrieR y'
  gtrieAlter (R1 k) f t@(STrieL x)      = case f Nothing of
                                            Nothing -> Just t
                                            Just v -> Just $! STrieB x (gtrieSingleton k v)
  gtrieAlter (R1 k) f (STrieB x y)      = case gtrieAlter k f y of
                                            Just y' -> Just $! STrieB x y'
                                            Nothing -> Just $! STrieL x

  gtrieMap f (STrieB x y)               = STrieB (gtrieMap f x) (gtrieMap f y)
  gtrieMap f (STrieL x)                 = STrieL (gtrieMap f x)
  gtrieMap f (STrieR y)                 = STrieR (gtrieMap f y)

  gtrieTraverse f (STrieB x y)          = liftA2 STrieB (gtrieTraverse f x) (gtrieTraverse f y)
  gtrieTraverse f (STrieL x)            = fmap STrieL (gtrieTraverse f x)
  gtrieTraverse f (STrieR y)            = fmap STrieR (gtrieTraverse f y)

  gmapMaybeWithKey f (STrieL x)         = fmap STrieL $! gmapMaybeWithKey (f . L1) x
  gmapMaybeWithKey f (STrieR y)         = fmap STrieR $! gmapMaybeWithKey (f . R1) y
  gmapMaybeWithKey f (STrieB x y)       = case (gmapMaybeWithKey (f . L1) x, gmapMaybeWithKey (f . R1) y) of
                                            (Nothing, Nothing) -> Nothing
                                            (Just x', Nothing) -> Just $! STrieL x'
                                            (Nothing, Just y') -> Just $! STrieR y'
                                            (Just x', Just y') -> Just $! STrieB x' y'

  gtraverseMaybeWithKey f (STrieL x)         = fmap STrieL <$> gtraverseMaybeWithKey (f . L1) x
  gtraverseMaybeWithKey f (STrieR y)         = fmap STrieR <$> gtraverseMaybeWithKey (f . R1) y
  gtraverseMaybeWithKey f (STrieB x y)       =
    liftA2 finish (gtraverseMaybeWithKey (f . L1) x) (gtraverseMaybeWithKey (f . R1) y)
    where
      finish Nothing   Nothing    = Nothing
      finish (Just x') Nothing    = Just $! STrieL x'
      finish Nothing   (Just y')  = Just $! STrieR y'
      finish (Just x') (Just y')  = Just $! STrieB x' y'

  gfoldWithKey f z (STrieL x)           = gfoldWithKey (f . L1) z x
  gfoldWithKey f z (STrieR y)           = gfoldWithKey (f . R1) z y
  gfoldWithKey f z (STrieB x y)         = gfoldWithKey (f . L1) (gfoldWithKey (f . R1) z y) x

  gtraverseWithKey f (STrieL x)         = fmap STrieL (gtraverseWithKey (f . L1) x)
  gtraverseWithKey f (STrieR y)         = fmap STrieR (gtraverseWithKey (f . R1) y)
  gtraverseWithKey f (STrieB x y)       = liftA2 STrieB (gtraverseWithKey (f . L1) x)
                                                        (gtraverseWithKey (f . R1) y)

  gmergeWithKey f g h x0 y0 =
    case (split x0, split y0) of
      ((xl,xr),(yl,yr)) -> build (mergel xl yl) (merger xr yr)
    where
    split (STrieL x)   = (Just x, Nothing)
    split (STrieR y)   = (Nothing, Just y)
    split (STrieB x y) = (Just x, Just y)

    build (Just x) (Just y) = Just (STrieB x y)
    build (Just x) Nothing  = Just (STrieL x)
    build Nothing  (Just y) = Just (STrieR y)
    build Nothing  Nothing  = Nothing

    mergel Nothing  Nothing  = Nothing
    mergel (Just x) Nothing  = gl x
    mergel Nothing  (Just y) = hl y
    mergel (Just x) (Just y) = gmergeWithKey (f . L1) gl hl x y

    merger Nothing  Nothing  = Nothing
    merger (Just x) Nothing  = gr x
    merger Nothing  (Just y) = hr y
    merger (Just x) (Just y) = gmergeWithKey (f . R1) gr hr x y

    gl t = do STrieL t' <- g (STrieL t)
              return t'
    gr t = do STrieR t' <- g (STrieR t)
              return t'
    hl t = do STrieL t' <- h (STrieL t)
              return t'
    hr t = do STrieR t' <- h (STrieR t)
              return t'

  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtraverseMaybeWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}

instance (GTrieKeyShow f, GTrieKeyShow g) => GTrieKeyShow (f :+: g) where
  gtrieShowsPrec p (STrieB x y)         = showParen (p > 10)
                                        $ showString "STrieB "
                                        . showsPrec 11 x
                                        . showString " "
                                        . showsPrec 11 y
  gtrieShowsPrec p (STrieL x)           = showParen (p > 10)
                                        $ showString "STrieL "
                                        . showsPrec 11 x
  gtrieShowsPrec p (STrieR y)           = showParen (p > 10)
                                        $ showString "STrieR "
                                        . showsPrec 11 y

------------------------------------------------------------------------------
-- Generic implementation for units
------------------------------------------------------------------------------

-- | Tries of constructors without fields are represented by a single value.
instance GTrieKey U1 where
  gtrieLookup _ (UTrie x)       = Just x
  gtrieInsert _ v _             = UTrie v
  gtrieDelete _ _               = Nothing
  gtrieAlter _ f (UTrie x)      = case f (Just x) of
                                    Nothing -> Nothing
                                    Just x' -> Just $! UTrie x'
  gtrieSingleton _              = UTrie
  gtrieMap f (UTrie x)          = UTrie (f x)
  gtrieTraverse f (UTrie x)     = fmap UTrie (f x)
  gmapMaybeWithKey f (UTrie x)  = fmap UTrie $! f U1 x
  gtraverseMaybeWithKey f (UTrie x)  = fmap (fmap UTrie) $! f U1 x
  gfoldWithKey f z (UTrie x)    = f U1 x z
  gtraverseWithKey f (UTrie x)  = fmap UTrie (f U1 x)
  gmergeWithKey f _ _ (UTrie x) (UTrie y) = fmap UTrie $! f U1 x y
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtraverseMaybeWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}

instance GTrieKeyShow U1 where
  gtrieShowsPrec p (UTrie x)    = showsPrec p x

------------------------------------------------------------------------------
-- Generic implementation for empty types
------------------------------------------------------------------------------

-- | Tries of types without constructors are represented by an empty type.
instance GTrieKey V1 where
-- Why is this represented by an empty type? One might expect it would
-- be represented by a unit type, as there is exactly one total function
-- from any empty type to any other type. First, remember that
-- TrieRepDefault offers an EmptyTrie constructor. So a TrieMap Void x
-- will be represented by that. Next, note that while the generic Rep
-- types can be put together in arbitrary ways, derived Generic instances (which
-- are the only ones that matter) are always structured as sums of products,
-- and only use V1 at the outermost level. That is, V1 will only appear in a
-- generic representation if it is the only thing there (aside from M1
-- wrappers). In particular, the only GTrie types that contain V1 are ones
-- for empty types, which are adequately represented by EmptyTrie. Indeed,
-- if we offered an inhabited GTrie for a Void type, we'd run into trouble,
-- because then we'd falsely claim that the TrieMap from Void isn't null!
  gtrieLookup _ t               = case t of
  gtrieInsert _ _ t             = case t of
  gtrieDelete _ t               = case t of
  gtrieAlter _ _ t              = case t of
  gtrieSingleton k _            = case k of
  gtrieMap _ t                  = case t of
  gtrieTraverse _ t             = case t of
  gmapMaybeWithKey _ t          = case t of
  gfoldWithKey _ _ t            = case t of
  gtraverseWithKey _ t          = case t of
  gtraverseMaybeWithKey _ t     = case t of
  gmergeWithKey _ _ _ t _       = case t of
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtraverseMaybeWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}

instance GTrieKeyShow V1 where
  gtrieShowsPrec _ _            = showString "()"


------------------------------------------------------------------------------
-- Various instances for Trie
------------------------------------------------------------------------------

instance (TrieKey k, Show k, Show a) => Show (Trie k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance (Show a, GTrieKeyShow f) => Show (GTrie f a) where
  showsPrec = gtrieShowsPrec

instance TrieKey k => Functor (Trie k) where
  fmap = trieMap

instance TrieKey k => Foldable.Foldable (Trie k) where
  foldMap = Traversable.foldMapDefault
  foldr f = trieFoldWithKey (\_ -> f)
#if MIN_VERSION_base(4,8,0)
  null = trieNull
#endif

instance TrieKey k => Traversable (Trie k) where
  traverse = trieTraverse
