{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-} -- coerce

-- | Unstable implementation details
module Data.GenericTrie.Internal
  ( TrieKey(..)
  , Trie(..)
  , OrdKey(..)
  -- * Generic derivation implementation
  , genericTrieNull
  , genericTrieMap
  , genericTrieTraverse
  , genericTrieShowsPrec
  , genericInsert
  , genericLookup
  , genericDelete
  , genericMapMaybeWithKey
  , genericSingleton
  , genericEmpty
  , genericFoldWithKey
  , genericTraverseWithKey
  , TrieRepDefault
  , GTrieKey(..)
  , GTrie(..)
  ) where

import Control.Applicative (Applicative, liftA2)
import Data.Char (chr, ord)
import Data.Coerce (coerce)
import Data.Foldable (Foldable)
import Data.Functor.Compose (Compose(..))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Traversable (Traversable,traverse)
import GHC.Generics
import qualified Data.Foldable as Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Prelude

-- | Keys that support prefix-trie map operations.
--
-- All operations can be automatically derived from a 'Generic' instance.
class TrieKey k where

  -- | Type of the representation of tries for this key.
  type TrieRep k :: * -> *

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

  trieAt :: Functor f => k -> (Maybe a -> f (Maybe a)) ->
                              Trie k a -> f (Trie k a)

  -- | Construct a trie holding a single value
  trieSingleton :: k -> a -> Trie k a

  -- | Apply a function to all values stored in a trie
  trieMap :: (a -> b) -> Trie k a -> Trie k b

  -- | Traverse the values stored in a trie
  trieTraverse :: Applicative f => (a -> f b) -> Trie k a -> f (Trie k b)

  -- | Show the representation of a trie
  trieShowsPrec :: Show a => Int -> Trie k a -> ShowS

  -- | Apply a function to the values of a 'Trie' and keep the elements
  -- of the trie that result in a 'Just' value.
  trieMapMaybeWithKey :: (k -> a -> Maybe b) -> Trie k a -> Trie k b

  -- | Fold a trie with a function of both key and value.
  trieFoldWithKey :: (k -> a -> r -> r) -> r -> Trie k a -> r

  -- | Traverse a trie with a function of both key and value.
  trieTraverseWithKey :: Applicative f => (k -> a -> f b) -> Trie k a -> f (Trie k b)

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

  default trieMap ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k) =>
    (a -> b) -> Trie k a -> Trie k b
  trieMap = genericTrieMap

  default trieTraverse ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k , Applicative f) =>
    (a -> f b) -> Trie k a -> f (Trie k b)
  trieTraverse = genericTrieTraverse

  default trieShowsPrec ::
    ( Show a, GTrieKeyShow (Rep k) , TrieRep k ~ TrieRepDefault k) =>
    Int -> Trie k a -> ShowS
  trieShowsPrec = genericTrieShowsPrec

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

  default trieMergeWithKey ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k, Generic k ) =>
    (k -> a -> b -> Maybe c) ->
    (Trie k a -> Trie k c) ->
    (Trie k b -> Trie k c) ->
    Trie k a -> Trie k b -> Trie k c
  trieMergeWithKey = genericMergeWithKey

  default trieAt ::
    ( GTrieKey (Rep k) , TrieRep k ~ TrieRepDefault k, Generic k, Functor f ) =>
    k -> (Maybe a -> f (Maybe a)) -> Trie k a -> f (Trie k a)
  trieAt = genericAt

-- | The default implementation of a 'TrieRep' is 'GTrie' wrapped in
-- a 'Maybe'. This wrapping is due to the 'GTrie' being a non-empty
-- trie allowing all the of the "emptiness" to be represented at the
-- top level for any given generically implemented key.
type TrieRepDefault k = Compose Maybe (GTrie (Rep k))

-- | Effectively an associated datatype of tries indexable by keys of type @k@.
-- By using a separate newtype wrapper around the associated type synonym we're
-- able to use the same 'MkTrie' constructor for all of the generic
-- implementations while still getting the injectivity of a new type.
newtype Trie k a = MkTrie (TrieRep k a)


------------------------------------------------------------------------------
-- Manually derived instances for base types
------------------------------------------------------------------------------

simpleAt ::
  (Functor f, TrieKey k) =>
  k -> (Maybe a -> f (Maybe a)) -> Trie k a -> f (Trie k a)
simpleAt k f m = flip fmap (f mv) $ \r -> case r of
    Nothing -> maybe m (const (trieDelete k m)) mv
    Just v' -> trieInsert k v' m
    where mv = trieLookup k m
{-# INLINE simpleAt #-}

-- | 'Int' tries are implemented with 'IntMap'.
instance TrieKey Int where
  type TrieRep Int              = IntMap
  trieLookup k (MkTrie x)       = IntMap.lookup k x
  trieInsert k v (MkTrie t)     = MkTrie (IntMap.insert k v t)
  trieDelete k (MkTrie t)       = MkTrie (IntMap.delete k t)
  trieEmpty                     = MkTrie IntMap.empty
  trieSingleton k v             = MkTrie (IntMap.singleton k v)
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey f x)
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldWithKey f z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey f x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey f (coerce g) (coerce h) x y)
  trieAt                        = simpleAt
  {-# INLINE trieEmpty #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieSingleton #-}
  {-# INLINE trieFoldWithKey #-}
  {-# INLINE trieShowsPrec #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieTraverseWithKey #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieMergeWithKey #-}
  {-# INLINE trieMapMaybeWithKey #-}

-- | 'Integer' tries are implemented with 'Map'.
instance TrieKey Integer where
  type TrieRep Integer              = Map Integer
  trieLookup k (MkTrie t)           = Map.lookup k t
  trieInsert k v (MkTrie t)         = MkTrie (Map.insert k v t)
  trieDelete k (MkTrie t)           = MkTrie (Map.delete k t)
  trieEmpty                         = MkTrie Map.empty
  trieSingleton k v                 = MkTrie (Map.singleton k v)
  trieNull (MkTrie x)               = Map.null x
  trieMap f (MkTrie x)              = MkTrie (Map.map f x)
  trieTraverse f (MkTrie x)         = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)        = showsPrec p x
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (Map.mapMaybeWithKey f x)
  trieFoldWithKey f z (MkTrie x)    = Map.foldrWithKey f z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (Map.traverseWithKey f x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (Map.mergeWithKey f (coerce g) (coerce h) x y)
  trieAt                            = simpleAt
  {-# INLINE trieEmpty #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieSingleton #-}
  {-# INLINE trieFoldWithKey #-}
  {-# INLINE trieShowsPrec #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieTraverseWithKey #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieMergeWithKey #-}
  {-# INLINE trieMapMaybeWithKey #-}

-- | 'Char tries are implemented with 'IntMap'.
instance TrieKey Char where
  type TrieRep Char                 = IntMap
  trieLookup k (MkTrie t)           = IntMap.lookup (ord k) t
  trieDelete k (MkTrie t)           = MkTrie (IntMap.delete (ord k) t)
  trieInsert k v (MkTrie t)         = MkTrie (IntMap.insert (ord k) v t)
  trieEmpty                         = MkTrie IntMap.empty
  trieSingleton k v                 = MkTrie (IntMap.singleton (ord k) v)
  trieNull (MkTrie x)               = IntMap.null x
  trieMap f (MkTrie x)              = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)         = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)        = showsPrec p x
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey (f . chr) x)
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldrWithKey (f . chr) z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey (f . chr) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey (f . chr) (coerce g) (coerce h) x y)
  trieAt                            = simpleAt
  {-# INLINE trieEmpty #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieSingleton #-}
  {-# INLINE trieFoldWithKey #-}
  {-# INLINE trieShowsPrec #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieTraverseWithKey #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieMergeWithKey #-}
  {-# INLINE trieMapMaybeWithKey #-}

-- | Force the use of a type's 'Ord' instance in order to use 'Map' as the
-- underlying trie representation directly.
newtype OrdKey k = OrdKey { getOrdKey :: k }
  deriving (Read, Show, Eq, Ord)

-- | 'OrdKey' tries are implemented with 'Map', this is
-- intended for cases where it is better for some reason
-- to force the use of a 'Map' than to use the generically
-- derived structure.
instance (Show k, Ord k) => TrieKey (OrdKey k) where
  type TrieRep (OrdKey k)               = Map k
  trieLookup (OrdKey k) (MkTrie x)      = Map.lookup k x
  trieInsert (OrdKey k) v (MkTrie x)    = MkTrie (Map.insert k v x)
  trieDelete (OrdKey k) (MkTrie x)      = MkTrie (Map.delete k x)
  trieEmpty                             = MkTrie Map.empty
  trieSingleton (OrdKey k) v            = MkTrie (Map.singleton k v)
  trieNull (MkTrie x)                   = Map.null x
  trieMap f (MkTrie x)                  = MkTrie (Map.map f x)
  trieTraverse f (MkTrie x)             = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)            = showsPrec p x
  trieMapMaybeWithKey f (MkTrie x)      = MkTrie (Map.mapMaybeWithKey (f . OrdKey) x)
  trieFoldWithKey f z (MkTrie x)        = Map.foldrWithKey (f . OrdKey) z x
  trieTraverseWithKey f (MkTrie x)      = fmap MkTrie (Map.traverseWithKey (f . OrdKey) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (Map.mergeWithKey (f . OrdKey) (coerce g) (coerce h) x y)
  trieAt                                = simpleAt
  {-# INLINE trieEmpty #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieSingleton #-}
  {-# INLINE trieFoldWithKey #-}
  {-# INLINE trieShowsPrec #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieTraverseWithKey #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieMergeWithKey #-}
  {-# INLINE trieMapMaybeWithKey #-}

------------------------------------------------------------------------------
-- Automatically derived instances for common types
------------------------------------------------------------------------------

instance                                      TrieKey ()
instance                                      TrieKey Bool
instance TrieKey k                         => TrieKey (Maybe k)
instance (TrieKey a, TrieKey b)            => TrieKey (Either a b)
instance (TrieKey a, TrieKey b)            => TrieKey (a,b)
instance (TrieKey a, TrieKey b, TrieKey c) => TrieKey (a,b,c)
instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d) => TrieKey (a,b,c,d)
instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d, TrieKey e) => TrieKey (a,b,c,d,e)
instance TrieKey k                         => TrieKey [k]

------------------------------------------------------------------------------
-- Generic 'TrieKey' method implementations
------------------------------------------------------------------------------

-- | Generic implementation of 'lookup'. This is the default implementation.
genericLookup ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> Trie k a -> Maybe a
genericLookup k (MkTrie (Compose t)) = gtrieLookup (from k) =<< t
{-# INLINABLE genericLookup #-}

-- | Generic implementation of 'trieNull'. This is the default implementation.
genericTrieNull ::
    ( TrieRep k ~ TrieRepDefault k
    ) =>
    Trie k a -> Bool
genericTrieNull (MkTrie (Compose mb)) = isNothing mb
{-# INLINABLE genericTrieNull #-}

-- | Generic implementation of 'singleton'. This is the default implementation.
genericSingleton ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> a -> Trie k a
genericSingleton k v = MkTrie $ Compose $ Just $! gtrieSingleton (from k) v
{-# INLINABLE genericSingleton #-}

-- | Generic implementation of 'empty'. This is the default implementation.
genericEmpty ::
    ( TrieRep k ~ TrieRepDefault k
    ) =>
    Trie k a
genericEmpty = MkTrie (Compose Nothing)
{-# INLINABLE genericEmpty #-}

-- | Generic implementation of 'insert'. This is the default implementation.
genericAt ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    , Functor f
    ) =>
    k -> (Maybe a -> f (Maybe a)) -> Trie k a -> f (Trie k a)
genericAt k f (MkTrie (Compose m)) =
  case m of
    Nothing -> fmap (MkTrie . Compose . fmap (gtrieSingleton (from k))) (f Nothing)
    Just t  -> gtrieAt (MkTrie . Compose) (from k) f t
{-# INLINABLE genericAt #-}

-- | Generic implementation of 'insert'. This is the default implementation.
genericInsert ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> a -> Trie k a -> Trie k a
genericInsert k v (MkTrie (Compose m)) = MkTrie $ Compose $
  case m of
    Nothing -> Just $! gtrieSingleton (from k) v
    Just t  -> Just $! gtrieInsert    (from k) v t
{-# INLINABLE genericInsert #-}

-- | Generic implementation of 'delete'. This is the default implementation.
genericDelete ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    k -> Trie k a -> Trie k a
genericDelete k (MkTrie (Compose m)) = MkTrie (Compose (gtrieDelete (from k) =<< m))
{-# INLINABLE genericDelete #-}

-- | Generic implementation of 'trieMap'. This is the default implementation.
genericTrieMap ::
    ( GTrieKey (Rep k)
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (a -> b) -> Trie k a -> Trie k b
genericTrieMap f (MkTrie (Compose x)) = MkTrie (Compose (fmap (gtrieMap f) $! x))
{-# INLINABLE genericTrieMap #-}


-- | Generic implementation of 'trieTraverse'. This is the default implementation.
genericTrieTraverse ::
    ( GTrieKey (Rep k)
    , TrieRep k ~ TrieRepDefault k
    , Applicative f
    ) =>
    (a -> f b) -> Trie k a -> f (Trie k b)
genericTrieTraverse f (MkTrie (Compose x)) =
  fmap (MkTrie . Compose) (traverse (gtrieTraverse f) x)
{-# INLINABLE genericTrieTraverse #-}

-- | Generic implementation of 'trieShowsPrec'. This is the default implementation.
genericTrieShowsPrec ::
    ( Show a, GTrieKeyShow (Rep k)
    , TrieRep k ~ TrieRepDefault k
    ) =>
    Int -> Trie k a -> ShowS
genericTrieShowsPrec p (MkTrie (Compose m)) =
  case m of
    Just x  -> showsPrec p x
    Nothing -> showString "()"
{-# INLINABLE genericTrieShowsPrec #-}

-- | Generic implementation of 'mapMaybe'. This is the default implementation.
genericMapMaybeWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (k -> a -> Maybe b) -> Trie k a -> Trie k b
genericMapMaybeWithKey f (MkTrie (Compose x)) = MkTrie (Compose (gmapMaybeWithKey (f . to) =<< x))
{-# INLINABLE genericMapMaybeWithKey #-}

-- | Generic implementation of 'foldWithKey'. This is the default implementation.
genericFoldWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (k -> a -> r -> r) -> r -> Trie k a -> r
genericFoldWithKey f z (MkTrie (Compose m)) =
  case m of
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
genericTraverseWithKey f (MkTrie (Compose m)) = fmap (MkTrie . Compose) (traverse (gtraverseWithKey (f . to)) m)
{-# INLINABLE genericTraverseWithKey #-}

-- | Generic implementation of 'mergeWithKey'. This is the default implementation.
genericMergeWithKey ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k ~ TrieRepDefault k
    ) =>
    (k -> a -> b -> Maybe c) -> (Trie k a -> Trie k c) -> (Trie k b -> Trie k c) ->
    Trie k a -> Trie k b -> Trie k c
genericMergeWithKey f g h (MkTrie (Compose x)) (MkTrie (Compose y)) =
  case (x,y) of
    (Nothing, Nothing) -> MkTrie (Compose Nothing)
    (Just{} , Nothing) -> g (MkTrie (Compose x))
    (Nothing, Just{} ) -> h (MkTrie (Compose y))
    (Just x', Just y') -> MkTrie (Compose (gmergeWithKey (f . to) (aux g) (aux h) x' y'))
      where
      aux k t = case k (MkTrie (Compose (Just t))) of
                  MkTrie (Compose r) -> r
{-# INLINABLE genericMergeWithKey #-}


------------------------------------------------------------------------------
-- Generic implementation class
------------------------------------------------------------------------------

-- | Mapping of generic representation of keys to trie structures.
data    family   GTrie (f :: * -> *) a
newtype instance GTrie (M1 i c f) a     = MTrie (GTrie f a)
data    instance GTrie (f :+: g)  a     = STrieL !(GTrie f a) | STrieR !(GTrie g a)
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
  gtrieMap       :: (a -> b) -> GTrie f a -> GTrie f b
  gtrieTraverse  :: Applicative m => (a -> m b) -> GTrie f a -> m (GTrie f b)
  gmapMaybeWithKey :: (f p -> a -> Maybe b) -> GTrie f a -> Maybe (GTrie f b)
  gfoldWithKey   :: (f p -> a -> r -> r) -> r -> GTrie f a -> r
  gtraverseWithKey :: Applicative m => (f p -> a -> m b) -> GTrie f a -> m (GTrie f b)
  gmergeWithKey  :: (f p -> a -> b -> Maybe c) ->
                    (GTrie f a -> Maybe (GTrie f c)) ->
                    (GTrie f b -> Maybe (GTrie f c)) ->
                    GTrie f a -> GTrie f b -> Maybe (GTrie f c)
  gtrieAt        :: Functor m =>
                      (Maybe (GTrie f a) -> r) ->
                      f p ->
                      (Maybe a -> m (Maybe a)) ->
                      GTrie f a -> m r

-- | The 'GTrieKeyShow' class provides generic implementations
-- of 'showsPrec'. This class is separate due to its implementation
-- varying for diferent kinds of metadata.
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
  gtrieMap f (MTrie x)          = MTrie (gtrieMap f x)
  gtrieTraverse f (MTrie x)     = fmap MTrie (gtrieTraverse f x)
  gmapMaybeWithKey f (MTrie x)  = fmap MTrie (gmapMaybeWithKey (f . M1) x)
  gfoldWithKey f z (MTrie x)    = gfoldWithKey (f . M1) z x
  gtraverseWithKey f (MTrie x)  = fmap MTrie (gtraverseWithKey (f . M1) x)
  gmergeWithKey f g h (MTrie x) (MTrie y) = fmap MTrie (gmergeWithKey (f . M1) (coerce g) (coerce h) x y)
  gtrieAt z (M1 k) f (MTrie x)  = gtrieAt (z . fmap MTrie) k f x
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gtrieAt #-}

data MProxy c (f :: * -> *) a = MProxy

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
  gtrieMap f (KTrie x)                  = KTrie (trieMap f x)
  gtrieTraverse f (KTrie x)             = fmap KTrie (trieTraverse f x)
  gmapMaybeWithKey f (KTrie x)          = fmap KTrie (checkNull (trieMapMaybeWithKey (f . K1) x))
  gfoldWithKey f z (KTrie x)            = trieFoldWithKey (f . K1) z x
  gtraverseWithKey f (KTrie x)          = fmap KTrie (trieTraverseWithKey (f . K1) x)
  gmergeWithKey f g h (KTrie x) (KTrie y) = fmap KTrie (checkNull (trieMergeWithKey (f . K1) g' h' x y))
     where
     g' t = case g (KTrie t) of
              Just (KTrie t') -> t'
              Nothing         -> trieEmpty
     h' t = case h (KTrie t) of
              Just (KTrie t') -> t'
              Nothing         -> trieEmpty
  gtrieAt z (K1 k) f (KTrie x) = fmap (z . fmap KTrie . checkNull) (trieAt k f x)
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieAt #-}

instance TrieKey k => GTrieKeyShow (K1 i k) where
  gtrieShowsPrec p (KTrie x)            = showsPrec p x

------------------------------------------------------------------------------
-- Generic implementation for products
------------------------------------------------------------------------------

-- | Generic products are represented by tries of tries.
instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where

  gtrieLookup (i :*: j) (PTrie x)       = gtrieLookup j =<< gtrieLookup i x
  gtrieInsert (i :*: j) v (PTrie t)     = case gtrieLookup i t of
                                            Nothing -> PTrie (gtrieInsert i (gtrieSingleton j v) t)
                                            Just ti -> PTrie (gtrieInsert i (gtrieInsert j v ti) t)
  gtrieDelete (i :*: j) (PTrie t)       = case gtrieLookup i t of
                                            Nothing -> Just (PTrie t)
                                            Just ti -> case gtrieDelete j ti of
                                                         Nothing -> fmap PTrie $! gtrieDelete i t
                                                         Just tj -> Just (PTrie (gtrieInsert i tj t))
  gtrieSingleton (i :*: j) v            = PTrie (gtrieSingleton i (gtrieSingleton j v))
  gtrieMap f (PTrie x)                  = PTrie (gtrieMap (gtrieMap f) x)
  gtrieTraverse f (PTrie x)             = fmap PTrie (gtrieTraverse (gtrieTraverse f) x)
  gmapMaybeWithKey f (PTrie x)          = fmap PTrie (gmapMaybeWithKey (\i -> gmapMaybeWithKey (\j -> f (i:*:j))) x)
  gfoldWithKey f z (PTrie x)            = gfoldWithKey (\i m r -> gfoldWithKey (\j -> f (i:*:j)) r m) z x
  gtraverseWithKey f (PTrie x)          = fmap PTrie (gtraverseWithKey (\i ->
                                                      gtraverseWithKey (\j -> f (i :*: j))) x)
  gmergeWithKey f g h (PTrie x) (PTrie y) =
    fmap
      PTrie
      (gmergeWithKey
         (\i ->
           gmergeWithKey
             (\j -> f (i:*:j))
             (g' i)
             (h' i))
         (coerce g)
         (coerce h)
         x
         y)
    where
    g' i t = do PTrie t' <- g (PTrie (gtrieSingleton i t))
                gtrieLookup i t'
    h' i t = do PTrie t' <- h (PTrie (gtrieSingleton i t))
                gtrieLookup i t'

  gtrieAt z (i :*: j) f (PTrie t) = gtrieAt (z . fmap PTrie) i f1 t
    where
    f1 Nothing = fmap (fmap (gtrieSingleton j)) (f Nothing)
    f1 (Just ti) = gtrieAt id j f ti


  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieAt #-}

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

  gtrieDelete (L1 k) (STrieL x)         = fmap STrieL (gtrieDelete k x)
  gtrieDelete (L1 _) (STrieR y)         = Just (STrieR y)
  gtrieDelete (L1 k) (STrieB x y)       = case gtrieDelete k x of
                                            Nothing -> Just (STrieR y)
                                            Just x' -> Just (STrieB x' y)
  gtrieDelete (R1 _) (STrieL x)         = Just (STrieL x)
  gtrieDelete (R1 k) (STrieR y)         = fmap STrieR (gtrieDelete k y)
  gtrieDelete (R1 k) (STrieB x y)       = case gtrieDelete k y of
                                            Nothing -> Just (STrieL x)
                                            Just y' -> Just (STrieB x y')

  gtrieMap f (STrieB x y)               = STrieB (gtrieMap f x) (gtrieMap f y)
  gtrieMap f (STrieL x)                 = STrieL (gtrieMap f x)
  gtrieMap f (STrieR y)                 = STrieR (gtrieMap f y)

  gtrieTraverse f (STrieB x y)          = liftA2 STrieB (gtrieTraverse f x) (gtrieTraverse f y)
  gtrieTraverse f (STrieL x)            = fmap STrieL (gtrieTraverse f x)
  gtrieTraverse f (STrieR y)            = fmap STrieR (gtrieTraverse f y)

  gmapMaybeWithKey f (STrieL x)         = fmap STrieL (gmapMaybeWithKey (f . L1) x)
  gmapMaybeWithKey f (STrieR y)         = fmap STrieR (gmapMaybeWithKey (f . R1) y)
  gmapMaybeWithKey f (STrieB x y)       = case (gmapMaybeWithKey (f . L1) x, gmapMaybeWithKey (f . R1) y) of
                                            (Nothing, Nothing) -> Nothing
                                            (Just x', Nothing) -> Just (STrieL x')
                                            (Nothing, Just y') -> Just (STrieR y')
                                            (Just x', Just y') -> Just (STrieB x' y')

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

  gtrieAt z (L1 k) f (STrieL x)   = gtrieAt (z . fmap STrieL) k f x
  gtrieAt z (R1 k) f (STrieR y)   = gtrieAt (z . fmap STrieR) k f y
  gtrieAt z (L1 k) f (STrieR y)   = fmap (z . Just . maybe (STrieR y) (\v -> STrieB (gtrieSingleton k v) y)) (f Nothing)
  gtrieAt z (R1 k) f (STrieL x)   = fmap (z . Just . maybe (STrieL x) (\v -> STrieB x (gtrieSingleton k v))) (f Nothing)
  gtrieAt z (L1 k) f (STrieB x y) = gtrieAt (z . Just . maybe (STrieR y) (`STrieB` y)) k f x
  gtrieAt z (R1 k) f (STrieB x y) = gtrieAt (z . Just . maybe (STrieL x) (x `STrieB`)) k f y

  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieAt #-}

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
  gtrieSingleton _              = UTrie
  gtrieMap f (UTrie x)          = UTrie (f x)
  gtrieTraverse f (UTrie x)     = fmap UTrie (f x)
  gmapMaybeWithKey f (UTrie x)  = fmap UTrie (f U1 x)
  gfoldWithKey f z (UTrie x)    = f U1 x z
  gtraverseWithKey f (UTrie x)  = fmap UTrie (f U1 x)
  gmergeWithKey f _ _ (UTrie x) (UTrie y) = fmap UTrie (f U1 x y)
  gtrieAt z _ f (UTrie x)       = fmap (z . fmap UTrie) (f (Just x))
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieAt #-}

instance GTrieKeyShow U1 where
  gtrieShowsPrec p (UTrie x)    = showsPrec p x

------------------------------------------------------------------------------
-- Generic implementation for empty types
------------------------------------------------------------------------------

-- | Tries of types without constructors are represented by a unit.
instance GTrieKey V1 where
  gtrieLookup k t               = k `seq` t `seq` error "GTrieKey.V1: gtrieLookup"
  gtrieInsert k _ t             = k `seq` t `seq` error "GTrieKey.V1: gtrieInsert"
  gtrieDelete k t               = k `seq` t `seq` error "GTrieKey.V1: gtrieDelete"
  gtrieSingleton k _            = k `seq` error "GTrieKey.V1: gtrieSingleton"
  gtrieMap _ t                  = t `seq` error "GTrieKey.V1: gtrieMap"
  gtrieTraverse _ t             = t `seq` error "GTrieKey.V1: gtrieTraverse"
  gmapMaybeWithKey _ t          = t `seq` error "GTrieKey.V1: gmapMaybeWithKey"
  gfoldWithKey _ _ t            = t `seq` error "GTrieKey.V1: gmapFoldWithKey"
  gtraverseWithKey _ t          = t `seq` error "GTrieKey.V1: gtraverseWithKey"
  gmergeWithKey _ _ _ t u       = t `seq` u `seq` error "GTrieKey.V1: gmergeWithKey"
  gtrieAt _ k _ t               = k `seq` t `seq` error "GTrieKey.V1: gtrieAt"
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gfoldWithKey #-}
  {-# INLINE gtraverseWithKey #-}
  {-# INLINE gmergeWithKey #-}
  {-# INLINE gmapMaybeWithKey #-}
  {-# INLINE gtrieAt #-}

instance GTrieKeyShow V1 where
  gtrieShowsPrec _ _            = showString "()"


------------------------------------------------------------------------------
-- Various instances for Trie
------------------------------------------------------------------------------

instance (Show a, TrieKey  k) => Show (Trie  k a) where
  showsPrec = trieShowsPrec

instance (Show a, GTrieKeyShow f) => Show (GTrie f a) where
  showsPrec = gtrieShowsPrec

instance TrieKey k => Functor (Trie k) where
  fmap = trieMap

instance TrieKey k => Foldable (Trie k) where
  foldr f = trieFoldWithKey (\_ -> f)

instance TrieKey k => Traversable (Trie k) where
  traverse = trieTraverse
