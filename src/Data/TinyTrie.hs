{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{- |

All methods of 'TrieKey' can be derived automatically using
a 'Generic' instance.

@
data Demo = DemoC1 'Int' | DemoC2 'Int' 'Char'  deriving 'Generic'

instance 'TrieKey' Demo
@

* Example operations on 'Trie's
* @ 'view' . 'at' :: 'TrieKey' k => k -> 'Trie' k a -> 'Maybe' a @
* @ 'set'  . 'at' :: 'TrieKey' k => k ->             'Maybe' a  -> 'Trie' k a -> 'Trie' k a @
* @ 'over' . 'at' :: 'TrieKey' k => k -> ('Maybe' a -> 'Maybe' a) -> 'Trie' k a -> 'Trie' k a @
* @ 'toList'    :: 'TrieKey' k => 'Trie' k a -> [a] @
* @ 'itoList'   :: 'TrieKey' k => 'Trie' k a -> [(k,a)] @
-}

module Data.TinyTrie
  (
  -- * Trie data family
    Trie(..)
  -- * Trie operations
  , TrieKey(..)
  ) where



import Control.Applicative
import Control.Lens
import Data.Char (ord,chr)
import Data.Foldable (Foldable(..), toList)
import Data.Int
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Word
import GHC.Generics
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable


-- | Keys that support prefix-trie map operations.
--
-- All operations can be automatically derived from a 'Generic' instance.
class TrieKey k where

  -- | Type of the representation of tries for this key.
  type TrieRep k a
  type instance TrieRep k a = GTrie (Rep k) a


  -- | Returns 'True' when the 'Trie' contains no values.
  trieNull :: Trie k a -> Bool
  default trieNull ::
    (GTrieKey (Rep k) , TrieRep k a ~ GTrie (Rep k) a) =>
    Trie k a -> Bool
  trieNull (MkTrie x) = gtrieNull x


  -- | Returns a 'Trie' containing no values.
  trieEmpty :: Trie k a
  default trieEmpty ::
    ( GTrieKey (Rep k) , TrieRep k a ~ GTrie (Rep k) a) =>
    Trie k a
  trieEmpty = genericTrieEmpty


  -- | 'Lens' for visiting elements of the 'Trie'
  trieAt :: Functor f => k -> LensLike' f (Trie k a) (Maybe a)
  default trieAt ::
    ( GTrieKey (Rep k), Generic k, TrieRep k a ~ GTrie (Rep k) a, Functor f) =>
    k -> LensLike' f (Trie k a) (Maybe a)
  trieAt k f (MkTrie x) = fmap MkTrie (gtrieAt (GHC.Generics.from k) f x)


  trieShow :: Show a => Trie k a -> String
  default trieShow ::
    ( Show a , GTrieKey (Rep k) , TrieRep k a ~ GTrie (Rep k) a) =>
    Trie k a -> String
  trieShow (MkTrie x) = gtrieShow x

  {-# INLINE trieNull #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieAt #-}

-- | Effectively associated datatype of tries indexable by keys of type @k@.
newtype Trie k a = MkTrie (TrieRep k a)

-- I don't actually know why this needs to be its own definition, but
-- type checking fails if I inline it.
genericTrieEmpty ::
  ( GTrieKey (Rep k) , TrieRep k a ~ GTrie (Rep k) a) =>
  Trie k a
genericTrieEmpty = MkTrie gtrieEmpty
{-# INLINE genericTrieEmpty #-}

------------------------------------------------------------------------------
-- Manually derived instances for base types
------------------------------------------------------------------------------

instance TrieKey Int where
  type TrieRep Int a            = IntMap a
  trieAt k f (MkTrie x)         = fmap MkTrie (at k f x)
  trieNull (MkTrie x)           = IntMap.null x
  trieEmpty                     = MkTrie IntMap.empty
  trieShow (MkTrie x)           = show x
  {-# INLINE trieAt #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}

instance TrieKey Integer where
  type TrieRep Integer a        = Map Integer a
  trieAt k f (MkTrie x)         = fmap MkTrie (at k f x)
  trieNull (MkTrie x)           = Map.null x
  trieEmpty                     = MkTrie Map.empty
  trieShow (MkTrie x)           = show x
  {-# INLINE trieAt #-}
  {-# INLINE trieEmpty #-}
  {-# INLINE trieNull #-}

instance TrieKey Char where
  type TrieRep Char a           = IntMap a
  trieAt k f (MkTrie x)         = fmap MkTrie (at (ord k) f x)
  trieNull (MkTrie x)           = IntMap.null x
  trieEmpty                     = MkTrie IntMap.empty
  trieShow (MkTrie x)           = show x
  {-# INLINE trieAt #-}
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

-- | TrieKey operations on Generic representations used to provide
-- the default implementations of tries.
class GTrieKey f where
  data GTrie f a
  gtrieAt    :: Functor g => f () -> LensLike' g (GTrie f a) (Maybe a)
  gtrieNull  :: GTrie f a -> Bool
  gtrieEmpty :: GTrie f a
  gtrieShow  :: Show a => GTrie f a -> String

------------------------------------------------------------------------------
-- Generic implementation for metadata
------------------------------------------------------------------------------

instance GTrieKey f => GTrieKey (M1 i c f) where
  newtype GTrie (M1 i c f) a    = MTrie (GTrie f a)
  gtrieAt (M1 k) f (MTrie x)    = fmap MTrie (gtrieAt k f x)
  gtrieNull (MTrie x)           = gtrieNull x
  gtrieEmpty                    = MTrie gtrieEmpty
  gtrieShow (MTrie x)           = gtrieShow x
  {-# INLINE gtrieAt #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieEmpty #-}

------------------------------------------------------------------------------
-- Generic implementation for fields
------------------------------------------------------------------------------


instance TrieKey k => GTrieKey (K1 i k) where
  newtype GTrie (K1 i k) a      = KTrie (Trie k a)
  gtrieAt (K1 k) f (KTrie x)    = fmap KTrie (trieAt k f x)
  gtrieEmpty                    = KTrie trieEmpty
  gtrieNull (KTrie x)           = trieNull x
  gtrieShow (KTrie x)           = trieShow x
  {-# INLINE gtrieAt #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieEmpty #-}

------------------------------------------------------------------------------
-- Generic implementation for products
------------------------------------------------------------------------------

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where
  newtype GTrie (f :*: g) a = PTrie (GTrie f (GTrie g a))

  gtrieAt (i :*: j) f (PTrie x)
     = fmap PTrie (gtrieAt i (anon gtrieEmpty gtrieNull (gtrieAt j f)) x)

  gtrieEmpty                    = PTrie gtrieEmpty
  gtrieNull (PTrie x)           = gtrieNull x
  gtrieShow (PTrie x)           = gtrieShow x
  {-# INLINE gtrieAt #-}
  {-# INLINE gtrieNull #-}
  {-# INLINE gtrieEmpty #-}


------------------------------------------------------------------------------
-- Generic implementation for sums
------------------------------------------------------------------------------

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :+: g) where

  data GTrie (f :+: g) a        = STrie !(GTrie f a) !(GTrie g a)
  gtrieAt (L1 k) f (STrie x y)  = fmap (`STrie` y) (gtrieAt k f x)
  gtrieAt (R1 k) f (STrie x y)  = fmap (x `STrie`) (gtrieAt k f y)
  gtrieEmpty                    = STrie gtrieEmpty gtrieEmpty
  gtrieNull (STrie m1 m2)       = gtrieNull m1 && gtrieNull m2
  gtrieShow (STrie x y)         = "(" ++ gtrieShow x ++ ") + ("
                                      ++ gtrieShow y ++ ")"
  {-# INLINE gtrieAt #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}

------------------------------------------------------------------------------
-- Generic implementation for units
------------------------------------------------------------------------------

instance GTrieKey U1 where
  newtype GTrie U1 a            = UTrie (Maybe a)
  gtrieAt _ f (UTrie x)         = fmap UTrie (f x)
  gtrieEmpty                    = UTrie Nothing
  gtrieNull (UTrie u)           = isNothing u
  gtrieShow (UTrie u)           = show u
  {-# INLINE gtrieAt #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}

------------------------------------------------------------------------------
-- Generic implementation for empty types
------------------------------------------------------------------------------

instance GTrieKey V1 where
  data GTrie V1 a               = VTrie
  gtrieAt k _ _                 = k `seq` error "GTrieKey.V1: gtrieAt"
  gtrieEmpty                    = VTrie
  gtrieNull _                   = True
  gtrieShow _                   = "()"
  {-# INLINE gtrieAt #-}
  {-# INLINE gtrieEmpty #-}
  {-# INLINE gtrieNull #-}

------------------------------------------------------------------------------
-- Various instances for Trie
------------------------------------------------------------------------------

instance (Show a, TrieKey  k) => Show (Trie  k a) where show = trieShow
instance (Show a, GTrieKey f) => Show (GTrie f a) where show = gtrieShow

type instance IxValue (Trie k a) = a
type instance Index   (Trie k a) = k

instance TrieKey k => Ixed (Trie k a) where
  ix = ixAt
  {-# INLINE ix #-}

instance TrieKey k => At (Trie k a) where
  at = trieAt
  {-# INLINE at #-}
