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
import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)


-- | Keys that support prefix-trie map operations.
--
-- All operations can be automatically derived from a 'Generic' instance.
class TrieKey k where

  -- | Type of the representation of tries for this key.
  type TrieRep k a

  -- | Half implementation of '_Empty' method
  trieEmpty :: Trie k a

  -- | Lookup element from trie
  trieLookup :: k -> Trie k a -> Maybe a

  trieAlter :: (Maybe a -> Maybe a) -> k -> Trie k a -> Trie k a

  trieMap :: (a -> b) -> Trie k a -> Trie k b

  trieFold :: (a -> b -> b) -> Trie k a -> b -> b

  -- | Implementation of 'show' method
  trieShowsPrec :: Show a => Int -> Trie k a -> ShowS


  -- Defaults using 'Generic'
  type instance TrieRep k a = GTrie (Rep k) a

  default trieEmpty ::
    (GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) => Trie k a
  trieEmpty = MkTrie gtrieEmpty

  default trieAlter ::
    ( GTrieKey (Rep k), Generic k, TrieRep k a ~ GTrie (Rep k) a) =>
    (Maybe a -> Maybe a) -> k -> Trie k a -> Trie k a
  trieAlter f k (MkTrie x) = MkTrie (gtrieAlter f (from k) x)

  default trieLookup ::
    ( GTrieKey (Rep k), Generic k, TrieRep k a ~ GTrie (Rep k) a) =>
    k -> Trie k a -> Maybe a
  trieLookup k (MkTrie x) = gtrieLookup (from k) x

  default trieMap ::
    ( GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a, TrieRep k b ~ GTrie (Rep k) b) =>
    (a -> b) -> Trie k a -> Trie k b
  trieMap f (MkTrie x) = MkTrie (gtrieMap f x)

  default trieFold ::
    ( GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) =>
    (a -> b -> b) -> Trie k a -> b -> b
  trieFold f (MkTrie x) = gtrieFold f x

  default trieShowsPrec ::
    (Show a, GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) =>
    Int -> Trie k a -> ShowS
  trieShowsPrec p (MkTrie x) = showsPrec p x


-- | Effectively associated datatype of tries indexable by keys of type @k@.
newtype Trie k a = MkTrie (TrieRep k a)


------------------------------------------------------------------------------
-- Manually derived instances for base types
------------------------------------------------------------------------------

instance TrieKey Int where
  type TrieRep Int a            = IntMap a
  trieLookup k (MkTrie x)       = IntMap.lookup k x
  trieAlter f k (MkTrie x)      = MkTrie (IntMap.alter f k x)
  trieEmpty                     = MkTrie IntMap.empty
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieFold f (MkTrie x) z       = IntMap.foldr f z x
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

instance TrieKey Integer where
  type TrieRep Integer a        = Map Integer a
  trieLookup k   (MkTrie x)     = Map.lookup k x
  trieAlter f k (MkTrie x)      = MkTrie (Map.alter f k x)
  trieEmpty                     = MkTrie Map.empty
  trieMap f (MkTrie x)          = MkTrie (Map.map f x)
  trieFold f (MkTrie x) z       = Map.foldr f z x
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

instance TrieKey Char where
  type TrieRep Char a           = IntMap a
  trieLookup k (MkTrie x)       = IntMap.lookup (ord k) x
  trieAlter f k (MkTrie x)      = MkTrie (IntMap.alter f (ord k) x)
  trieEmpty                     = MkTrie IntMap.empty
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieFold f (MkTrie x) z       = IntMap.foldr f z x
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

newtype OrdKey k = OrdKey k
instance (Show k, Ord k) => TrieKey (OrdKey k) where
  type TrieRep (OrdKey k) a             = Map k a
  trieLookup (OrdKey k) (MkTrie x)      = Map.lookup k x
  trieAlter f (OrdKey k) (MkTrie x)     = MkTrie (Map.alter f k x)
  trieEmpty                             = MkTrie Map.empty
  trieMap f (MkTrie x)                  = MkTrie (Map.map f x)
  trieFold f (MkTrie x) z               = Map.foldr f z x
  trieShowsPrec p (MkTrie x)            = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

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
data    instance GTrie (f :+: g)  a     = STrie !(GTrie f a) !(GTrie g a) | STrie0
newtype instance GTrie (f :*: g)  a     = PTrie (GTrie f (GTrie g a))
newtype instance GTrie (K1 i k)   a     = KTrie (Trie k a)
newtype instance GTrie U1         a     = UTrie (Maybe a)
data    instance GTrie V1         a     = VTrie

-- | TrieKey operations on Generic representations used to provide
-- the default implementations of tries.
class GTrieKey f where
  gtrieLookup    :: f () -> GTrie f a -> Maybe a
  gtrieAlter     :: (Maybe a -> Maybe a) -> f () -> GTrie f a -> GTrie f a
  gtrieEmpty     :: GTrie f a
  gtrieMap       :: (a -> b) -> GTrie f a -> GTrie f b
  gtrieFold      :: (a -> b -> b) -> GTrie f a -> b -> b
  gtrieShowsPrec :: Show a => Int -> GTrie f a -> ShowS

------------------------------------------------------------------------------
-- Generic implementation for metadata
------------------------------------------------------------------------------

instance GTrieKey f => GTrieKey (M1 i c f) where
  gtrieLookup (M1 k) (MTrie x)  = gtrieLookup k x
  gtrieAlter f (M1 k) (MTrie x) = MTrie (gtrieAlter f k x)
  gtrieEmpty                    = MTrie gtrieEmpty
  gtrieMap f (MTrie x)          = MTrie (gtrieMap f x)
  gtrieFold f (MTrie x)         = gtrieFold f x
  gtrieShowsPrec p (MTrie x)    = showsPrec p x
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieShowsPrec #-}

------------------------------------------------------------------------------
-- Generic implementation for fields
------------------------------------------------------------------------------


instance TrieKey k => GTrieKey (K1 i k) where
  gtrieLookup (K1 k) (KTrie x)          = trieLookup k x
  gtrieAlter f (K1 k) (KTrie x)         = KTrie (trieAlter f k x)
  gtrieEmpty                            = KTrie trieEmpty
  gtrieMap f (KTrie x)                  = KTrie (trieMap f x)
  gtrieFold f (KTrie x )                = trieFold f x
  gtrieShowsPrec p (KTrie x)            = showsPrec p x
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieShowsPrec #-}

------------------------------------------------------------------------------
-- Generic implementation for products
------------------------------------------------------------------------------

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where

  gtrieLookup (i :*: j) (PTrie x)       = gtrieLookup j =<< gtrieLookup i x

  gtrieAlter f (i :*: j) (PTrie x)      = PTrie (gtrieAlter alterJ i x)
    where
    -- possible laziness issue here
    alterJ Nothing                      = Just (gtrieAlter f j gtrieEmpty)
    alterJ (Just t)                     = Just (gtrieAlter f j t)

  gtrieEmpty                            = PTrie gtrieEmpty
  gtrieMap f (PTrie x)                  = PTrie (gtrieMap (gtrieMap f) x)
  gtrieFold f (PTrie x)                 = gtrieFold (gtrieFold f) x
  gtrieShowsPrec p (PTrie x)            = showsPrec p x
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieShowsPrec #-}


------------------------------------------------------------------------------
-- Generic implementation for sums
------------------------------------------------------------------------------

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :+: g) where

  gtrieLookup _      STrie0             = Nothing
  gtrieLookup (L1 k) (STrie x y)        = gtrieLookup k x
  gtrieLookup (R1 k) (STrie x y)        = gtrieLookup k y

  gtrieAlter f k      STrie0            = gtrieAlter f k gtrieEmpty
  gtrieAlter f (L1 k) (STrie x y)       = STrie (gtrieAlter f k x) y
  gtrieAlter f (R1 k) (STrie x y)       = STrie x (gtrieAlter f k y)

  gtrieEmpty                            = STrie gtrieEmpty gtrieEmpty

  gtrieMap _ STrie0                     = STrie0
  gtrieMap f (STrie x y)                = STrie (gtrieMap f x) (gtrieMap f y)

  gtrieFold _ STrie0                    = id
  gtrieFold f (STrie x y)               = gtrieFold f x . gtrieFold f y

  gtrieShowsPrec _ STrie0               = showString "STrie0"
  gtrieShowsPrec p (STrie x y)          = showParen (p > 10)
                                        $ showString "STrie "
                                        . showsPrec 11 x
                                        . showString " "
                                        . showsPrec 11 y
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieShowsPrec #-}

------------------------------------------------------------------------------
-- Generic implementation for units
------------------------------------------------------------------------------

instance GTrieKey U1 where
  gtrieLookup _ (UTrie x)       = x
  gtrieAlter f _ (UTrie x)      = UTrie (f x)
  gtrieEmpty                    = UTrie Nothing
  gtrieMap f (UTrie x)          = UTrie (fmap f x)
  gtrieFold _ (UTrie Nothing)   = id
  gtrieFold f (UTrie (Just x))  = f x
  gtrieShowsPrec p (UTrie x)    = showsPrec p x
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieShowsPrec #-}

------------------------------------------------------------------------------
-- Generic implementation for empty types
------------------------------------------------------------------------------

instance GTrieKey V1 where
  gtrieLookup k _               = k `seq` error "GTrieKey.V1: gtrieLookup"
  gtrieAlter _ k _              = k `seq` error "GTrieKey.V1: gtrieAlter"
  gtrieEmpty                    = VTrie
  gtrieMap _ _                  = VTrie
  gtrieFold _ _                 = id
  gtrieShowsPrec _ _            = showString "VTrie"
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieShowsPrec #-}

------------------------------------------------------------------------------
-- Various helpers
------------------------------------------------------------------------------

insert :: TrieKey k => k -> v -> Trie k v -> Trie k v
insert k v = trieAlter (const (Just v)) k

delete :: TrieKey k => k -> Trie k v -> Trie k v
delete = trieAlter (const Nothing)

------------------------------------------------------------------------------
-- Various instances for Trie
------------------------------------------------------------------------------

instance (Show a, TrieKey  k) => Show (Trie  k a) where
  showsPrec = trieShowsPrec

instance (Show a, GTrieKey f) => Show (GTrie f a) where
  showsPrec = gtrieShowsPrec

instance TrieKey k => Functor (Trie k) where
  fmap = trieMap

instance TrieKey k => Foldable (Trie k) where
  foldr f z t = trieFold f t z
