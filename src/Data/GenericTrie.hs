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

module Data.GenericTrie
  (
  -- * Trie data family
    Trie(..)
  , lookup
  , alter
  , insert
  , delete
  , fromList
  -- * Instance implementation details
  , TrieKey(..)
  -- * Generic implementation details
  , GTrieKey(..)
  , GTrie(..)
  ) where


import Data.Char (ord)
import Data.Foldable (Foldable)
import Data.IntMap (IntMap)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing)
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
  trieEmpty :: Trie k a

  -- | Test for an empty trie
  trieNull :: Trie k a -> Bool

  -- | Lookup element from trie
  trieLookup :: k -> Trie k a -> Maybe a

  -- | Alter the value at a key.
  trieAlter :: (Maybe a -> Maybe a) -> k -> Trie k a -> Trie k a

  -- | Apply a function to all values stored in a trie
  trieMap :: (a -> b) -> Trie k a -> Trie k b

  -- | Fold all the values store in a trie
  trieFold :: (a -> b -> b) -> Trie k a -> b -> b

  -- | Show the representation of a trie
  trieShowsPrec :: Show a => Int -> Trie k a -> ShowS


  -- Defaults using 'Generic'

  type instance TrieRep k a = GTrie (Rep k) a

  default trieEmpty ::
    (GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) => Trie k a
  trieEmpty = MkTrie gtrieEmpty

  default trieNull ::
    (GTrieKey (Rep k), TrieRep k a ~ GTrie (Rep k) a) => Trie k a -> Bool
  trieNull (MkTrie x) = gtrieNull x

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

  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}


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
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieFold f (MkTrie x) z       = IntMap.foldr f z x
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

instance TrieKey Integer where
  type TrieRep Integer a        = Map Integer a
  trieLookup k   (MkTrie x)     = Map.lookup k x
  trieAlter f k (MkTrie x)      = MkTrie (Map.alter f k x)
  trieEmpty                     = MkTrie Map.empty
  trieNull (MkTrie x)           = Map.null x
  trieMap f (MkTrie x)          = MkTrie (Map.map f x)
  trieFold f (MkTrie x) z       = Map.foldr f z x
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

instance TrieKey Char where
  type TrieRep Char a           = IntMap a
  trieLookup k (MkTrie x)       = IntMap.lookup (ord k) x
  trieAlter f k (MkTrie x)      = MkTrie (IntMap.alter f (ord k) x)
  trieEmpty                     = MkTrie IntMap.empty
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieFold f (MkTrie x) z       = IntMap.foldr f z x
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}

newtype OrdKey k = OrdKey k
instance (Show k, Ord k) => TrieKey (OrdKey k) where
  type TrieRep (OrdKey k) a             = Map k a
  trieLookup (OrdKey k) (MkTrie x)      = Map.lookup k x
  trieAlter f (OrdKey k) (MkTrie x)     = MkTrie (Map.alter f k x)
  trieEmpty                             = MkTrie Map.empty
  trieNull (MkTrie x)                   = Map.null x
  trieMap f (MkTrie x)                  = MkTrie (Map.map f x)
  trieFold f (MkTrie x) z               = Map.foldr f z x
  trieShowsPrec p (MkTrie x)            = showsPrec p x
  {-# INLINE trieLookup #-}
  {-# INLINE trieAlter #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}
  {-# INLINE trieMap #-}
  {-# INLINE trieFold #-}
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
  gtrieLookup    :: f p -> GTrie f a -> Maybe a
  gtrieAlter     :: (Maybe a -> Maybe a) -> f p -> GTrie f a -> GTrie f a
  gtrieEmpty     :: GTrie f a
  gtrieNull      :: GTrie f a -> Bool
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
  gtrieNull (MTrie x)           = gtrieNull x
  gtrieMap f (MTrie x)          = MTrie (gtrieMap f x)
  gtrieFold f (MTrie x)         = gtrieFold f x
  gtrieShowsPrec p (MTrie x)    = showsPrec p x
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}
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
  gtrieNull (KTrie x)                   = trieNull x
  gtrieMap f (KTrie x)                  = KTrie (trieMap f x)
  gtrieFold f (KTrie x )                = trieFold f x
  gtrieShowsPrec p (KTrie x)            = showsPrec p x
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}
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
    alterJ m = let m' = gtrieAlter f j (fromMaybe gtrieEmpty m)
               in if gtrieNull m' then Nothing else Just m'

  gtrieEmpty                            = PTrie gtrieEmpty
  gtrieNull (PTrie x)                   = gtrieNull x
  gtrieMap f (PTrie x)                  = PTrie (gtrieMap (gtrieMap f) x)
  gtrieFold f (PTrie x)                 = gtrieFold (gtrieFold f) x
  gtrieShowsPrec p (PTrie x)            = showsPrec p x
  {-# INLINE gtrieAlter #-}
  {-# INLINE gtrieLookup #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieMap #-}
  {-# INLINE gtrieFold #-}
  {-# INLINE gtrieShowsPrec #-}


------------------------------------------------------------------------------
-- Generic implementation for sums
------------------------------------------------------------------------------

strie :: (GTrieKey f, GTrieKey g) => GTrie f a -> GTrie g a -> GTrie (f :+: g) a
strie x y
  | gtrieNull x && gtrieNull y = STrie0
  | otherwise                  = STrie x y

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :+: g) where

  gtrieLookup (L1 k) (STrie x _)        = gtrieLookup k x
  gtrieLookup (R1 k) (STrie _ y)        = gtrieLookup k y
  gtrieLookup _      STrie0             = Nothing

  gtrieAlter f (L1 k) (STrie x y)       = strie (gtrieAlter f k x) y
  gtrieAlter f (R1 k) (STrie x y)       = strie x (gtrieAlter f k y)
  gtrieAlter f (L1 k) STrie0            = strie (gtrieAlter f k gtrieEmpty) gtrieEmpty
  gtrieAlter f (R1 k) STrie0            = strie gtrieEmpty (gtrieAlter f k gtrieEmpty)

  gtrieEmpty                            = STrie0

  gtrieNull STrie0                      = True
  gtrieNull STrie{}                     = False

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
  {-# INLINE gtrieNull #-}
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
  gtrieNull (UTrie x)           = isNothing x
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
  gtrieNull _                   = True
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

-- | Lookup an element in a trie
lookup :: TrieKey k => k -> Trie k v -> Maybe v
lookup = trieLookup
{-# INLINE lookup #-}

-- | Alter an element in a trie. The function is given the previous
-- value, if it exists, and returns Just to set a value and Nothing to delete
alter :: TrieKey k => (Maybe v -> Maybe v) -> k -> Trie k v -> Trie k v
alter = trieAlter
{-# INLINE alter #-}

-- | Insert an element into the trie at the given key
insert :: TrieKey k => k -> v -> Trie k v -> Trie k v
insert k v = trieAlter (const (Just v)) k
{-# INLINE insert #-}

-- | Delete the element in a trie at the given key
delete :: TrieKey k => k -> Trie k v -> Trie k v
delete = trieAlter (const Nothing)
{-# INLINE delete #-}

-- | Construct a trie from a list of key/value pairs
fromList :: TrieKey k => [(k,v)] -> Trie k v
fromList = foldl' (\acc (k,v) -> insert k v acc) trieEmpty
{-# INLINE fromList #-}

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
