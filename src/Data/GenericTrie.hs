{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    Trie(..)
  , alter
  , fromList
  , TrieKey(..)
  -- * Generic derivation implementation
  , TrieRepDefault
  , GTrieKey(..)
  , GTrie(..)
  ) where


import Control.Applicative (Applicative, liftA2, pure)
import Data.Char (ord)
import Data.Foldable (Foldable)
import Data.IntMap (IntMap)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Traversable (Traversable,traverse)
import GHC.Generics
import Prelude hiding (lookup)
import qualified Data.Foldable as Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map


-- | Keys that support prefix-trie map operations.
--
-- All operations can be automatically derived from a 'Generic' instance.
class TrieKey k where

  -- | Type of the representation of tries for this key.
  type TrieRep k a

  -- | Construct an empty trie
  empty :: Trie k a

  -- | Test for an empty trie
  trieNull :: Trie k a -> Bool

  -- | Lookup element from trie
  lookup :: k -> Trie k a -> Maybe a

  -- | Insert element into trie
  insert :: k -> a -> Trie k a -> Trie k a

  -- | Delete element from trie
  delete :: k -> Trie k a -> Trie k a

  -- | Apply a function to all values stored in a trie
  trieMap :: (a -> b) -> Trie k a -> Trie k b

  -- | Fold all the values store in a trie
  trieFold :: (a -> b -> b) -> Trie k a -> b -> b

  -- | Traverse the values stored in a trie
  trieTraverse :: Applicative f => (a -> f b) -> Trie k a -> f (Trie k b)

  -- | Show the representation of a trie
  trieShowsPrec :: Show a => Int -> Trie k a -> ShowS


  -- Defaults using 'Generic'

  type instance TrieRep k a = TrieRepDefault k a

  default empty ::
    ( TrieRep k a ~ TrieRepDefault k a
    ) =>
    Trie k a
  empty = MkTrie Nothing

  default trieNull ::
    ( TrieRep k a ~ TrieRepDefault k a
    ) =>
    Trie k a -> Bool
  trieNull (MkTrie mb) = isNothing mb

  default lookup ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k a ~ TrieRepDefault k a
    ) =>
    k -> Trie k a -> Maybe a
  lookup k (MkTrie t) = gtrieLookup (from k) =<< t

  default insert ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k a ~ TrieRepDefault k a
    ) =>
    k -> a -> Trie k a -> Trie k a
  insert k v (MkTrie Nothing)  = MkTrie (Just $! gtrieSingleton (from k) v)
  insert k v (MkTrie (Just t)) = MkTrie (Just $! gtrieInsert (from k) v t)

  default delete ::
    ( GTrieKey (Rep k), Generic k
    , TrieRep k a ~ TrieRepDefault k a
    ) =>
    k -> Trie k a -> Trie k a
  delete _ t@(MkTrie Nothing) = t
  delete k (MkTrie (Just t))  = MkTrie (gtrieDelete (from k) t)

  default trieMap ::
    ( GTrieKey (Rep k)
    , TrieRep k a ~ TrieRepDefault k a
    , TrieRep k b ~ TrieRepDefault k b
    ) =>
    (a -> b) -> Trie k a -> Trie k b
  trieMap f (MkTrie x) = MkTrie (fmap (gtrieMap f) $! x)

  default trieFold ::
    ( GTrieKey (Rep k)
    , TrieRep k a ~ TrieRepDefault k a
    ) =>
    (a -> b -> b) -> Trie k a -> b -> b
  trieFold f (MkTrie (Just x)) z = gtrieFold f x z
  trieFold _ (MkTrie Nothing) z = z

  default trieTraverse ::
    ( GTrieKey (Rep k)
    , TrieRep k a ~ TrieRepDefault k a
    , TrieRep k b ~ TrieRepDefault k b
    , Applicative f
    ) =>
    (a -> f b) -> Trie k a -> f (Trie k b)
  trieTraverse f (MkTrie x) = fmap MkTrie (traverse (gtrieTraverse f) x)

  default trieShowsPrec ::
    ( Show a, GTrieKeyShow (Rep k)
    , TrieRep k a ~ TrieRepDefault k a
    ) =>
    Int -> Trie k a -> ShowS
  trieShowsPrec p (MkTrie (Just x)) = showsPrec p x
  trieShowsPrec _ (MkTrie Nothing ) = showString "()"

-- | The default implementation of a 'TrieRep' is 'GTrie' wrapped in
-- a 'Maybe'. This wrapping is due to the 'GTrie' being a non-empty
-- trie allowing all the of the "emptiness" to be represented at the
-- top level for any given generically implemented key.
type TrieRepDefault k a = Maybe (GTrie (Rep k) a)

-- | Effectively an associated datatype of tries indexable by keys of type @k@.
-- By using a separate newtype wrapper around the associated type synonym we're
-- able to use the same 'MkTrie' constructor for all of the generic
-- implementations while still getting the injectivity of a new type.
newtype Trie k a = MkTrie (TrieRep k a)


------------------------------------------------------------------------------
-- Manually derived instances for base types
------------------------------------------------------------------------------

-- | 'Int' tries are implemented with 'IntMap'.
instance TrieKey Int where
  type TrieRep Int a            = IntMap a
  lookup k (MkTrie x)           = IntMap.lookup k x
  insert k v (MkTrie t)         = MkTrie (IntMap.insert k v t)
  delete k (MkTrie t)           = MkTrie (IntMap.delete k t)
  empty                         = MkTrie IntMap.empty
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieFold f (MkTrie x) z       = IntMap.foldr f z x
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE empty #-}
  {-# INLINE insert #-}
  {-# INLINE lookup #-}
  {-# INLINE delete #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

-- | 'Integer' tries are implemented with 'Map'.
instance TrieKey Integer where
  type TrieRep Integer a        = Map Integer a
  lookup k (MkTrie t)           = Map.lookup k t
  insert k v (MkTrie t)         = MkTrie (Map.insert k v t)
  delete k (MkTrie t)           = MkTrie (Map.delete k t)
  empty                         = MkTrie Map.empty
  trieNull (MkTrie x)           = Map.null x
  trieMap f (MkTrie x)          = MkTrie (Map.map f x)
  trieFold f (MkTrie x) z       = Map.foldr f z x
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE empty #-}
  {-# INLINE insert #-}
  {-# INLINE lookup #-}
  {-# INLINE delete #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}
  {-# INLINE trieTraverse #-}

-- | 'Char tries are implemented with 'IntMap'.
instance TrieKey Char where
  type TrieRep Char a           = IntMap a
  lookup k (MkTrie t)           = IntMap.lookup (ord k) t
  delete k (MkTrie t)           = MkTrie (IntMap.delete (ord k) t)
  insert k v (MkTrie t)         = MkTrie (IntMap.insert (ord k) v t)
  empty                         = MkTrie IntMap.empty
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieFold f (MkTrie x) z       = IntMap.foldr f z x
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE empty #-}
  {-# INLINE insert #-}
  {-# INLINE lookup #-}
  {-# INLINE delete #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

newtype OrdKey k = OrdKey k
  deriving (Read, Show, Eq, Ord)

-- | 'OrdKey' tries are implemented with 'Map', this is
-- intended for cases where it is better for some reason
-- to force the use of a 'Map' than to use the generically
-- derived structure.
instance (Show k, Ord k) => TrieKey (OrdKey k) where
  type TrieRep (OrdKey k) a             = Map k a
  lookup (OrdKey k) (MkTrie x)          = Map.lookup k x
  insert (OrdKey k) v (MkTrie x)        = MkTrie (Map.insert k v x)
  delete (OrdKey k) (MkTrie x)          = MkTrie (Map.delete k x)
  empty                                 = MkTrie Map.empty
  trieNull (MkTrie x)                   = Map.null x
  trieMap f (MkTrie x)                  = MkTrie (Map.map f x)
  trieFold f (MkTrie x) z               = Map.foldr f z x
  trieTraverse f (MkTrie x)             = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)            = showsPrec p x
  {-# INLINE empty #-}
  {-# INLINE insert #-}
  {-# INLINE lookup #-}
  {-# INLINE delete #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}
  {-# INLINE trieTraverse #-}
  {-# INLINE trieShowsPrec #-}

------------------------------------------------------------------------------
-- Automatically derived instances for common types
------------------------------------------------------------------------------

instance                                      TrieKey ()
instance                                      TrieKey Bool
instance TrieKey k                         => TrieKey (Maybe k)
instance (TrieKey a, TrieKey b)            => TrieKey (Either a b)
instance (TrieKey a, TrieKey b)            => TrieKey (a,b)
instance (TrieKey a, TrieKey b, TrieKey c) => TrieKey (a,b,c)
instance TrieKey k                         => TrieKey [k]

------------------------------------------------------------------------------
-- Generic implementation class
------------------------------------------------------------------------------

-- | Mapping of generic representation of keys to trie structures.
data    family   GTrie (f :: * -> *) a
newtype instance GTrie (M1 i c f) a     = MTrie { unMTrie :: GTrie f a }
data    instance GTrie (f :+: g)  a     = STrieL !(GTrie f a) | STrieR !(GTrie g a)
                                        | STrieB !(GTrie f a) !(GTrie g a)
newtype instance GTrie (f :*: g)  a     = PTrie (GTrie f (GTrie g a))
newtype instance GTrie (K1 i k)   a     = KTrie (Trie k a)
newtype instance GTrie U1         a     = UTrie a
data    instance GTrie V1         a     = VTrie

-- | TrieKey operations on Generic representations used to provide
-- the default implementations of tries.
class GTrieKey f where
  gtrieLookup    :: f p -> GTrie f a -> Maybe a
  gtrieInsert    :: f p -> a -> GTrie f a -> GTrie f a
  gtrieSingleton :: f p -> a -> GTrie f a
  gtrieDelete    :: f p -> GTrie f a -> Maybe (GTrie f a)
  gtrieMap       :: (a -> b) -> GTrie f a -> GTrie f b
  gtrieFold      :: (a -> b -> b) -> GTrie f a -> b -> b
  gtrieTraverse  :: Applicative m => (a -> m b) -> GTrie f a -> m (GTrie f b)

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
  gtrieFold f (MTrie x)         = gtrieFold f x
  gtrieTraverse f (MTrie x)     = fmap MTrie (gtrieTraverse f x)
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieTraverse #-}

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

-- | Generic fields are represented by tries of the field type.
instance TrieKey k => GTrieKey (K1 i k) where
  gtrieLookup (K1 k) (KTrie x)          = lookup k x
  gtrieInsert (K1 k) v (KTrie t)        = KTrie (insert k v t)
  gtrieSingleton (K1 k) v               = KTrie (insert k v empty)
  gtrieDelete (K1 k) (KTrie t)          = let m = delete k t
                                          in if trieNull m then Nothing
                                                           else Just (KTrie m)
  gtrieMap f (KTrie x)                  = KTrie (trieMap f x)
  gtrieFold f (KTrie x )                = trieFold f x
  gtrieTraverse f (KTrie x)             = fmap KTrie (traverse f x)
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieTraverse #-}

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
  gtrieFold f (PTrie x)                 = gtrieFold (gtrieFold f) x
  gtrieTraverse f (PTrie x)             = fmap PTrie (gtrieTraverse (gtrieTraverse f) x)
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieTraverse #-}

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

  gtrieFold f (STrieB x y)              = gtrieFold f x . gtrieFold f y
  gtrieFold f (STrieL x)                = gtrieFold f x
  gtrieFold f (STrieR y)                = gtrieFold f y

  gtrieTraverse f (STrieB x y)          = liftA2 STrieB (gtrieTraverse f x) (gtrieTraverse f y)
  gtrieTraverse f (STrieL x)            = fmap STrieL (gtrieTraverse f x)
  gtrieTraverse f (STrieR y)            = fmap STrieR (gtrieTraverse f y)

  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gtrieMap #-}

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
  gtrieFold f (UTrie x)         = f x
  gtrieTraverse f (UTrie x)     = fmap UTrie (f x)
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieTraverse #-}
  {-# INLINE gtrieMap #-}

instance GTrieKeyShow U1 where
  gtrieShowsPrec p (UTrie x)    = showsPrec p x

------------------------------------------------------------------------------
-- Generic implementation for empty types
------------------------------------------------------------------------------

-- | Tries of types without constructors are represented by a unit.
instance GTrieKey V1 where
  gtrieLookup k _               = k `seq` error "GTrieKey.V1: gtrieLookup"
  gtrieInsert k _ _             = k `seq` error "GTrieKey.V1: gtrieInsert"
  gtrieDelete k _               = k `seq` error "GTrieKey.V1: gtrieDelete"
  gtrieSingleton k _            = k `seq` error "GTrieKey.V1: gtrieSingleton"
  gtrieMap _ _                  = VTrie
  gtrieFold _ _                 = id
  gtrieTraverse _ _             = pure VTrie
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieSingleton #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieTraverse #-}

instance GTrieKeyShow V1 where
  gtrieShowsPrec _ _            = showString "()"

------------------------------------------------------------------------------
-- Various helpers
------------------------------------------------------------------------------

-- | Construct a trie from a list of key/value pairs
fromList :: TrieKey k => [(k,v)] -> Trie k v
fromList = foldl' (\acc (k,v) -> insert k v acc) empty
{-# INLINE fromList #-}

-- | Alter the values of a trie. The function will take the value stored
-- as the given key if one exists and should return a value to insert at
-- that location or Nothing to delete from that location.
alter :: TrieKey k => k -> (Maybe a -> Maybe a) -> Trie k a -> Trie k a
alter k f t =
  case f (lookup k t) of
    Just v' -> insert k v' t
    Nothing -> delete k t

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
  foldr f z t = trieFold f t z

instance TrieKey k => Traversable (Trie k) where
  traverse = trieTraverse
