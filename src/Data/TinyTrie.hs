{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

{- |

All methods of 'TrieKey' can be derived automatically using
a 'Generic' instance.

@
data Demo = DemoC1 'Int' | DemoC2 'Int' 'Char'  deriving 'Generic'

instance 'TrieKey' Demo
@

-}

module Data.TinyTrie
  (
  -- * Trie data family
    Trie(..)
  -- * Instance implementation details
  , TrieKey(..)
  -- * Generic implementation details
  , GTrieKey(..)
  , GTrie(..)
  ) where


import Data.Char (ord)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (isNothing)
import GHC.Generics
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map


-- | Keys that support prefix-trie map operations.
--
-- All operations can be automatically derived from a 'Generic' instance.
class TrieKey k where

  -- | Type of the representation of tries for this key.
  type TrieRep k a

  -- | Half implementation of '_Empty' method
  trieNull :: Trie k a -> Bool

  -- | Half implementation of '_Empty' method
  trieEmpty :: Trie k a

  -- | Lookup element from trie
  trieLookup :: k -> Trie k a -> Maybe a

  -- | Insert element into trie
  trieInsert :: k -> a -> Trie k a -> Trie k a

  -- | Delete element from trie
  trieDelete :: k -> Trie k a -> Trie k a

  -- | Implementation of 'show' method
  trieShowsPrec :: Show a => Int -> Trie k a -> ShowS


  -- Defaults using 'Generic'
  type instance TrieRep k a = GTrie (Rep k) a

  default trieNull ::
    (GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) => Trie k a -> Bool
  trieNull (MkTrie x) = gtrieNull x

  default trieEmpty ::
    (GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) => Trie k a
  trieEmpty = MkTrie gtrieEmpty

  default trieInsert ::
    ( GTrieKey (Rep k), Generic k, TrieRep k a ~ GTrie (Rep k) a) =>
    k -> a -> Trie k a -> Trie k a
  trieInsert k v (MkTrie x) = MkTrie (gtrieInsert (from k) v x)

  default trieLookup ::
    ( GTrieKey (Rep k), Generic k, TrieRep k a ~ GTrie (Rep k) a) =>
    k -> Trie k a -> Maybe a
  trieLookup k (MkTrie x) = gtrieLookup (from k) x

  default trieDelete ::
    ( GTrieKey (Rep k), Generic k, TrieRep k a ~ GTrie (Rep k) a) =>
    k -> Trie k a -> Trie k a
  trieDelete k (MkTrie x) = MkTrie (gtrieDelete (from k) x)

  default trieShowsPrec ::
    (Show a, GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) =>
    Int -> Trie k a -> ShowS
  trieShowsPrec p (MkTrie x) = showsPrec p x

  {-# INLINE trieNull #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieLookup #-}


-- | Effectively associated datatype of tries indexable by keys of type @k@.
newtype Trie k a = MkTrie (TrieRep k a)


------------------------------------------------------------------------------
-- Manually derived instances for base types
------------------------------------------------------------------------------

instance TrieKey Int where
  type TrieRep Int a            = IntMap a
  trieLookup k   (MkTrie x)     =         IntMap.lookup k   x
  trieDelete k   (MkTrie x)     = MkTrie (IntMap.delete k   x)
  trieInsert k v (MkTrie x)     = MkTrie (IntMap.insert k v x)
  trieNull (MkTrie x)           = IntMap.null x
  trieEmpty                     = MkTrie IntMap.empty
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}

instance TrieKey Integer where
  type TrieRep Integer a        = Map Integer a
  trieLookup k   (MkTrie x)     =         Map.lookup k   x
  trieDelete k   (MkTrie x)     = MkTrie (Map.delete k   x)
  trieInsert k v (MkTrie x)     = MkTrie (Map.insert k v x)
  trieNull (MkTrie x)           = Map.null x
  trieEmpty                     = MkTrie Map.empty
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}

instance TrieKey Char where
  type TrieRep Char a           = IntMap a
  trieLookup k   (MkTrie x)     =         IntMap.lookup (ord k)   x
  trieDelete k   (MkTrie x)     = MkTrie (IntMap.delete (ord k)   x)
  trieInsert k v (MkTrie x)     = MkTrie (IntMap.insert (ord k) v x)
  trieNull (MkTrie x)           = IntMap.null x
  trieEmpty                     = MkTrie IntMap.empty
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieDelete #-}
  {-# INLINE trieInsert #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}

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

-- | Generic Trie structures
data    family   GTrie (f :: * -> *) a
newtype instance GTrie (M1 i c f) a     = MTrie (GTrie f a)
data    instance GTrie (f :+: g)  a     = STrie !(GTrie f a) !(GTrie g a)
newtype instance GTrie (f :*: g)  a     = PTrie (GTrie f (GTrie g a))
newtype instance GTrie (K1 i k)   a     = KTrie (Trie k a)
newtype instance GTrie U1         a     = UTrie (Maybe a)
data    instance GTrie V1         a     = VTrie

-- | TrieKey operations on Generic representations used to provide
-- the default implementations of tries.
class GTrieKey f where
  gtrieLookup    :: f () -> GTrie f a -> Maybe a
  gtrieInsert    :: f () -> a -> GTrie f a -> GTrie f a
  gtrieDelete    :: f () -> GTrie f a -> GTrie f a
  gtrieNull      :: GTrie f a -> Bool
  gtrieEmpty     :: GTrie f a
  gtrieShowsPrec :: Show a => Int -> GTrie f a -> ShowS

------------------------------------------------------------------------------
-- Generic implementation for metadata
------------------------------------------------------------------------------

instance GTrieKey f => GTrieKey (M1 i c f) where
  gtrieLookup (M1 k)   (MTrie x) = gtrieLookup k x
  gtrieInsert (M1 k) v (MTrie x) = MTrie (gtrieInsert k v x)
  gtrieDelete (M1 k)   (MTrie x) = MTrie (gtrieDelete k x)
  gtrieNull (MTrie x)           = gtrieNull x
  gtrieEmpty                    = MTrie gtrieEmpty
  gtrieShowsPrec p (MTrie x)    = showsPrec p x
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieEmpty #-}

------------------------------------------------------------------------------
-- Generic implementation for fields
------------------------------------------------------------------------------


instance TrieKey k => GTrieKey (K1 i k) where
  gtrieLookup (K1 k)   (KTrie x) = trieLookup k x
  gtrieInsert (K1 k) v (KTrie x) = KTrie (trieInsert k v x)
  gtrieDelete (K1 k)   (KTrie x) = KTrie (trieDelete k x)
  gtrieEmpty                    = KTrie trieEmpty
  gtrieNull (KTrie x)           = trieNull x
  gtrieShowsPrec p (KTrie x)    = showsPrec p x
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieEmpty #-}

------------------------------------------------------------------------------
-- Generic implementation for products
------------------------------------------------------------------------------

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where

  gtrieLookup (i :*: j) (PTrie x) = gtrieLookup j =<< gtrieLookup i x

  gtrieDelete (i :*: j) (PTrie x) = PTrie $
    case gtrieLookup i x of
      Nothing -> x
      Just y  ->
        let y' = gtrieDelete j y
        in if gtrieNull y'
             then gtrieDelete i x
             else gtrieInsert i y' x

  gtrieInsert (i :*: j) v (PTrie x) = PTrie $
    let y' = case gtrieLookup i x of
               Nothing -> gtrieInsert j v gtrieEmpty
               Just y  -> gtrieInsert j v y
    in y' `seq` gtrieInsert i y' x

  gtrieEmpty                    = PTrie gtrieEmpty
  gtrieNull (PTrie x)           = gtrieNull x
  gtrieShowsPrec p (PTrie x)    = showsPrec p x
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieEmpty #-}


------------------------------------------------------------------------------
-- Generic implementation for sums
------------------------------------------------------------------------------

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :+: g) where

  gtrieLookup (L1 k) (STrie x y) = gtrieLookup k x
  gtrieLookup (R1 k) (STrie x y) = gtrieLookup k y

  gtrieInsert (L1 k) v (STrie x y) = STrie (gtrieInsert k v x) y
  gtrieInsert (R1 k) v (STrie x y) = STrie x (gtrieInsert k v y)

  gtrieDelete (L1 k) (STrie x y) = STrie (gtrieDelete k x) y
  gtrieDelete (R1 k) (STrie x y) = STrie x (gtrieDelete k y)

  gtrieEmpty                    = STrie gtrieEmpty gtrieEmpty
  gtrieNull (STrie m1 m2)       = gtrieNull m1 && gtrieNull m2
  gtrieShowsPrec p (STrie x y)  = showParen (p > 10)
                                $ showString "STrie "
                                . showsPrec 11 x
                                . showString " "
                                . showsPrec 11 y
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}

------------------------------------------------------------------------------
-- Generic implementation for units
------------------------------------------------------------------------------

instance GTrieKey U1 where
  gtrieInsert _ v _             = UTrie (Just v)
  gtrieDelete _   _             = UTrie Nothing
  gtrieLookup _ (UTrie x)       = x
  gtrieEmpty                    = UTrie Nothing
  gtrieNull (UTrie x)           = isNothing x
  gtrieShowsPrec p (UTrie x)    = showsPrec p x
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}

------------------------------------------------------------------------------
-- Generic implementation for empty types
------------------------------------------------------------------------------

instance GTrieKey V1 where
  gtrieLookup k _               = k `seq` error "GTrieKey.V1: gtrieLookup"
  gtrieInsert k _ _             = k `seq` error "GTrieKey.V1: gtrieInsert"
  gtrieDelete k _               = k `seq` error "GTrieKey.V1: gtrieDelete"
  gtrieEmpty                    = VTrie
  gtrieNull _                   = True
  gtrieShowsPrec _ _            = showString "VTrie"
  {-# INLINE gtrieDelete #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieInsert #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}

------------------------------------------------------------------------------
-- Various instances for Trie
------------------------------------------------------------------------------

instance (Show a, TrieKey  k) => Show (Trie  k a) where
  showsPrec = trieShowsPrec

instance (Show a, GTrieKey f) => Show (GTrie f a) where
  showsPrec = gtrieShowsPrec
