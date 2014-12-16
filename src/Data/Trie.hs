{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{- |

By defining an instance of 'TrieKey' with a 'GenericTrie' implementation
for 'Trie' all methods of 'TrieKey' can be derived automatically.

> data DemoType = DemoC1 Int | DemoC2 Int Int
>   deriving Generic
>
> instance TrieKey DemoType where
>   newtype Trie DemoType a = DemoTrie (GenericTrie DemoType a)
>
-}

module Data.Trie
  (
  -- * Trie operations
    TrieKey(..)
  -- * Generic instance generation
  , GenericTrie
  ) where



import Data.Maybe (isNothing)
import Data.Char (ord,chr)
import Data.Coerce
import Data.Type.Coercion
import Control.Lens
import Control.Applicative
import Data.Monoid
import Data.Map (Map)
import GHC.Generics
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map


-- | Keys that support prefix-trie map operations.
class TrieKey k where
  -- | Associated datatype of tries indexable by keys of type @k@.
  --
  -- @
  -- Instances
  -- 'TrieKey' k => 'Functor'                ('Trie' k)
  -- 'TrieKey' k => 'Foldable'               ('Trie' k)
  -- 'TrieKey' k => 'Traversable'            ('Trie' k)
  -- 'TrieKey' k => 'TraversableWithIndex' k ('Trie' k)
  -- 'TrieKey' k => 'FunctorWithIndex'     k ('Trie' k)
  -- 'TrieKey' k => 'FoldableWithIndex'    k ('Trie' k)
  --
  -- 'Index'   ('Trie' k a) = k
  -- 'IxValue' ('Trie' k a) = a
  -- 'TrieKey' k => 'At'   ('Trie' k a)
  -- 'TrieKey' k => 'Ixed' ('Trie' k a)
  -- @
  data Trie k a

  -- | Returns 'True' when the 'Trie' contains no values.
  trieNull  :: Trie k a -> Bool
  default trieNull ::
    ( GTrieKey (Rep k)
    , Coercible (Trie k a) (GTrie (Rep k) a)
    ) =>
    Trie k a -> Bool
  trieNull = genericTrieNull

  -- | Returns a 'Trie' containing no values.
  trieEmpty :: Trie k a
  default trieEmpty ::
    ( GTrieKey (Rep k)
    , Generic k
    , Coercible (Trie k a) (GTrie (Rep k) a)
    ) => Trie k a
  trieEmpty = genericTrieEmpty

  -- | 'Lens' for visiting elements of the 'Trie'
  trieAt  :: k -> Lens' (Trie k a) (Maybe a)
  default trieAt ::
    ( GTrieKey (Rep k)
    , Generic k
    , Coercible (Trie k a) (GTrie (Rep k) a)
    ) =>
    k -> Lens' (Trie k a) (Maybe a)
  trieAt = genericTrieAt

  -- | Implementation of 'IndexedTraversal' used to implement
  -- 'TraversableWithIndex' and other classes listed above for all 'Trie's.
  trieITraverse :: IndexedTraversal k (Trie k a) (Trie k b) a b
  default trieITraverse ::
     ( Generic k
     , GTrieKey (Rep k)
     , Coercible (Trie k a) (GTrie (Rep k) a)
     , Coercible (Trie k b) (GTrie (Rep k) b)
     ) =>
     IndexedTraversal k (Trie k a) (Trie k b) a b
  trieITraverse = genericTrieITraverse

  -- | Implementation of the 'Monoid' 'mappend' function used in the 'Monoid'
  -- instance for 'Trie'.
  trieAppend :: Monoid a => Trie k a -> Trie k a -> Trie k a
  default trieAppend ::
     ( GTrieKey (Rep k)
     , Coercible (Trie k a) (GTrie (Rep k) a)
     , Monoid a
     ) =>
     Trie k a -> Trie k a -> Trie k a
  trieAppend = genericTrieAppend

instance TrieKey k => Functor     (Trie k) where fmap     = fmapDefault
instance TrieKey k => Foldable    (Trie k) where foldMap  = foldMapDefault
instance TrieKey k => Traversable (Trie k) where traverse = trieITraverse

instance (Monoid a, TrieKey k) => Monoid (Trie k a) where
  mappend = trieAppend
  mempty  = trieEmpty

type instance Index   (Trie k a) = k
type instance IxValue (Trie k a) = a
instance TrieKey k => At   (Trie k a) where at = trieAt
instance TrieKey k => Ixed (Trie k a) where ix = ixAt


-- | Abstract type when generating tries from the 'Generic' representation of a key.
newtype GenericTrie k a = GenericTrie (GTrie (Rep k) a)



-- Base instances

instance TrieKey Int where
  newtype Trie Int a          = IntTrie (IntMap a)
  trieAt k                    = iso (\(IntTrie t) -> t) IntTrie . at k
  trieNull (IntTrie x)        = IntMap.null x
  trieEmpty                   = IntTrie IntMap.empty
  trieITraverse f (IntTrie x) = fmap IntTrie (itraversed f x)
  trieAppend (IntTrie x) (IntTrie y) = IntTrie (IntMap.unionWith mappend x y)

instance TrieKey Integer where
  newtype Trie Integer a          = IntegerTrie (Map Integer a)
  trieAt k                      = iso (\(IntegerTrie t) -> t) IntegerTrie . at k
  trieNull (IntegerTrie x)        = Map.null x
  trieEmpty                       = IntegerTrie Map.empty
  trieITraverse f (IntegerTrie x) = fmap IntegerTrie (itraversed f x)
  trieAppend (IntegerTrie x) (IntegerTrie y) = IntegerTrie (Map.unionWith mappend x y)

instance TrieKey Char where
  newtype Trie Char a          = CharTrie (IntMap a)
  trieAt k                   = iso (\(CharTrie t) -> t) CharTrie . at (ord k)
  trieNull (CharTrie x)        = IntMap.null x
  trieEmpty                    = CharTrie IntMap.empty
  trieAppend (CharTrie x) (CharTrie y) = CharTrie (IntMap.unionWith mappend x y)
  trieITraverse f (CharTrie x) = fmap CharTrie (reindexed chr itraversed f x)

instance TrieKey Bool where
  data Trie Bool a             = BoolTrie !(Maybe a) !(Maybe a)
  trieAt False f (BoolTrie x y) = fmap (`BoolTrie` y) (f x)
  trieAt True  f (BoolTrie x y) = fmap (x `BoolTrie`) (f y)
  trieNull (BoolTrie x y)       = isNothing x && isNothing y
  trieEmpty                     = BoolTrie Nothing Nothing
  trieAppend (BoolTrie x1 x2) (BoolTrie y1 y2) = BoolTrie (mappend x1 y1) (mappend x2 y2)
  trieITraverse f (BoolTrie x y) = BoolTrie <$> traverse (indexed f False) x <*> traverse (indexed f True) y



instance TrieKey k => TrieKey (Maybe k) where
  newtype Trie (Maybe k) a = MaybeTrie { unMaybeTrie :: GenericTrie (Maybe k) a }



instance (TrieKey k1, TrieKey k2) => TrieKey (Either k1 k2) where
  newtype Trie (Either k1 k2) a = EitherTrie { unEitherTrie :: GenericTrie (Either k1 k2) a }



instance (TrieKey k1, TrieKey k2) => TrieKey (k1,k2) where
  newtype Trie (k1,k2) a = Tuple2Trie { unTuple2Trie :: GenericTrie (k1,k2) a }



instance TrieKey k => TrieKey [k] where
  newtype Trie [k] a = ListTrie { unListTrie :: GenericTrie [k] a }


genericTrieNull ::
  forall k a.
  ( GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  ) =>
  Trie k a -> Bool
genericTrieNull = coerceWith (sym Coercion) (gtrieNull :: GTrie (Rep k) a -> Bool)

genericTrieEmpty ::
  forall k a.
  ( Generic k
  , GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  ) =>
  Trie k a
genericTrieEmpty = coerceWith (sym Coercion) (gtrieEmpty :: GTrie (Rep k) a)


genericTrieAppend ::
  forall k a.
  ( GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  , Monoid a
  ) =>
  Trie k a -> Trie k a -> Trie k a
genericTrieAppend =
  case sym (Coercion :: Coercion (Trie k a) (GTrie (Rep k) a)) of
    Coercion ->
      Data.Coerce.coerce (gtrieAppend :: GTrie (Rep k) a -> GTrie (Rep k) a -> GTrie (Rep k) a)


genericTrieAt ::
  ( Generic k
  , GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  ) =>
  k -> Lens' (Trie k a) (Maybe a)
genericTrieAt k = iso Data.Coerce.coerce (coerceWith (sym Coercion)) . gtrieAt (GHC.Generics.from k)


genericTrieLens ::
  (Generic k, GTrieKey (Rep k)) =>
  (Trie k a -> GTrie (Rep k) a) ->
  (GTrie (Rep k) a -> Trie k a) ->
  k -> Lens' (Trie k a) (Maybe a)
genericTrieLens f g k = iso f g . gtrieAt (GHC.Generics.from k)


genericTrieITraverse ::
  forall a b k.
  ( Generic k, GTrieKey (Rep k)
  , Coercible (Trie k b) (GTrie (Rep k) b)
  , Coercible (Trie k a) (GTrie (Rep k) a)) =>
  IndexedTraversal k (Trie k a) (Trie k b) a b
genericTrieITraverse = iso Data.Coerce.coerce (coerceWith (sym Coercion)) . reindexed to' gtrieITraverse
  where
  to' :: Rep k () -> k
  to' = GHC.Generics.to



-- | TrieKey operations on Generic representations used to provide
-- the default implementations of tries.
class GTrieKey f where
  data GTrie f a
  gtrieAt      :: f () -> Lens' (GTrie f a) (Maybe a)
  gtrieNull      :: GTrie f a -> Bool
  gtrieEmpty     :: GTrie f a
  gtrieAppend    :: Monoid a => GTrie f a -> GTrie f a -> GTrie f a
  gtrieITraverse :: IndexedTraversal (f ()) (GTrie f a) (GTrie f b) a b



mtrieIso :: Iso (GTrie (M1 i c f) a) (GTrie (M1 i c f) b) (GTrie f a) (GTrie f b)
mtrieIso = iso (\(MTrie p) -> p) MTrie

instance GTrieKey f => GTrieKey (M1 i c f) where
  newtype GTrie (M1 i c f) a = MTrie (GTrie f a)
  gtrieAt (M1 k)             = mtrieIso . gtrieAt k
  gtrieNull (MTrie m)        = gtrieNull m
  gtrieEmpty                 = MTrie gtrieEmpty
  gtrieAppend (MTrie x) (MTrie y) = MTrie (gtrieAppend x y)
  gtrieITraverse             = mtrieIso . reindexed m1 gtrieITraverse
    where
    m1 :: f () -> M1 i c f ()
    m1 = M1


ktrieIso :: Iso (GTrie (K1 i k) a) (GTrie (K1 i k') b) (Trie k a) (Trie k' b)
ktrieIso = iso (\(KTrie p) -> p) KTrie

instance TrieKey k => GTrieKey (K1 i k) where
  newtype GTrie (K1 i k) a = KTrie (Trie k a)
  gtrieAt (K1 k)           = ktrieIso . trieAt k
  gtrieNull (KTrie k)      = trieNull k
  gtrieEmpty               = KTrie trieEmpty
  gtrieAppend (KTrie x) (KTrie y) = KTrie (trieAppend x y)
  gtrieITraverse           = ktrieIso . reindexed k1 trieITraverse
    where
    k1 :: a -> K1 i a ()
    k1 = K1


ptrieIso :: Iso (GTrie (f :*: g) a) (GTrie (f' :*: g') b) (GTrie f (GTrie g a)) (GTrie f' (GTrie g' b))
ptrieIso = iso (\(PTrie p) -> p) PTrie

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where
  newtype GTrie (f :*: g) a = PTrie (GTrie f (GTrie g a))

  gtrieAt (i :*: j)   = ptrieIso
                      . gtrieAt i
                      . anon gtrieEmpty gtrieNull
                      . gtrieAt j

  gtrieEmpty          = PTrie gtrieEmpty
  gtrieAppend (PTrie x) (PTrie y) = PTrie (gtrieAppend x y)
  gtrieNull (PTrie m) = gtrieNull m
  gtrieITraverse      = ptrieIso . icompose (:*:) gtrieITraverse gtrieITraverse


-- Actually used in the :*: case's gtrieAppend!
instance (GTrieKey k, Monoid a) => Monoid (GTrie k a) where
  mappend = gtrieAppend
  mempty  = gtrieEmpty


strieFst :: Lens (GTrie (f :+: g) a) (GTrie (f' :+: g) a) (GTrie f a) (GTrie f' a)
strieFst f (STrie a b) = fmap (`STrie` b) (f a)

strieSnd :: Lens (GTrie (f :+: g) a) (GTrie (f :+: g') a) (GTrie g a) (GTrie g' a)
strieSnd f (STrie a b) = fmap (a `STrie`) (f b)

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :+: g) where

  data GTrie (f :+: g) a       = STrie !(GTrie f a) !(GTrie g a)
  gtrieAt (L1 k)               = strieFst . gtrieAt k
  gtrieAt (R1 k)               = strieSnd . gtrieAt k
  gtrieEmpty                   = STrie gtrieEmpty gtrieEmpty
  gtrieNull (STrie m1 m2)      = gtrieNull m1 && gtrieNull m2
  gtrieAppend (STrie m1 m2) (STrie n1 n2) = STrie (gtrieAppend m1 n1) (gtrieAppend m2 n2)
  gtrieITraverse f (STrie x y) = STrie <$> reindexed l1 gtrieITraverse f x
                                       <*> reindexed r1 gtrieITraverse f y
    where
    l1 :: f () -> (f :+: g) ()
    l1 = L1
    r1 :: g () -> (f :+: g) ()
    r1 = R1


utrieIso :: Iso (GTrie U1 a) (GTrie U1 b) (Maybe a) (Maybe b)
utrieIso = iso (\(UTrie x) -> x) UTrie

instance GTrieKey U1 where
  newtype GTrie U1 a  = UTrie (Maybe a)
  gtrieAt _           = utrieIso
  gtrieEmpty          = UTrie Nothing
  gtrieNull (UTrie u) = isNothing u
  gtrieAppend (UTrie x) (UTrie y) = UTrie (x <> y)
  gtrieITraverse      = utrieIso . traverse . flip indexed u1
    where
    u1 :: U1 ()
    u1 = U1


instance GTrieKey V1 where
  data GTrie V1 a        = VTrie
  gtrieAt k _ _          = k `seq` error "GTrieKey.V1: gtrieAt"
  gtrieEmpty             = VTrie
  gtrieAppend _ _        = VTrie
  gtrieNull _            = True
  gtrieITraverse _ _     = pure VTrie


instance TrieKey k => FunctorWithIndex     k (Trie k) where
instance TrieKey k => FoldableWithIndex    k (Trie k) where
instance TrieKey k => TraversableWithIndex k (Trie k) where
  itraverse  = trieITraverse . Indexed
  itraversed = trieITraverse

foldFromFoldMap :: (Applicative f, Contravariant f) => ((a -> f a) -> s -> f a) -> (a -> f a) -> s -> f s
foldFromFoldMap myFoldMap f s = Control.Lens.coerce (myFoldMap f s)
