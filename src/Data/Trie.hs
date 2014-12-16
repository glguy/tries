{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{- |

By defining an instance of 'TrieKey' with a 'GenericTrie' implementation
for 'Trie' all methods of 'TrieKey' can be derived automatically.

@
data Demo = DemoC1 'Int' | DemoC2 'Int' 'Char'  deriving 'Generic'

instance 'TrieKey' Demo
newtype instance 'Trie' Demo a = DemoTrie ('GenericTrie' Demo a)
@

* Example operations on 'Trie's
* @ 'view' . 'at' :: 'TrieKey' k => k -> 'Trie' k a -> 'Maybe' a @
* @ 'set'  . 'at' :: 'TrieKey' k => k ->             'Maybe' a  -> 'Trie' k a -> 'Trie' k a @
* @ 'over' . 'at' :: 'TrieKey' k => k -> ('Maybe' a -> 'Maybe' a) -> 'Trie' k a -> 'Trie' k a @
* @ 'toList'    :: 'TrieKey' k => 'Trie' k a -> [a] @
* @ 'itoList'   :: 'TrieKey' k => 'Trie' k a -> [(k,a)] @
-}

module Data.Trie
  (
  -- * Trie data family
    Trie
  -- * Trie operations
  , TrieKey(..)
  -- * Generic instance generation
  , GenericTrie
  ) where



import Control.Applicative
import Control.Lens
import Data.Char (ord,chr)
import Data.Coerce
import Data.Foldable (Foldable(..), toList)
import Data.Int
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Type.Coercion
import Data.Word
import GHC.Generics
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable


-- | Associated datatype of tries indexable by keys of type @k@.
data family Trie k a

type instance Index   (Trie k a) = k
type instance IxValue (Trie k a) = a

-- | Keys that support prefix-trie map operations.
--
-- All operations can be automatically derived when
-- the associated 'Trie' type is a /newtype/ of 'GenericTrie'.
class TrieKey k where

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

  -- | Expose operation like 'Map.mergeWithKey' and 'IntMap.mergeWithKey' used
  -- to provide efficient operations on two 'Trie's.
  -- When using this function care must be taken to ensure that
  -- the second and third function arguments return a 'Trie' with a
  -- subset of the original keys. No new keys may be added, but fewer may
  -- be returned.
  trieMergeWithKey :: (k -> a -> b -> Maybe c) -> (Trie k a -> Trie k c) -> (Trie k b -> Trie k c) -> Trie k a -> Trie k b -> Trie k c
  default trieMergeWithKey ::
     ( GTrieKey (Rep k)
     , Generic k
     , Coercible (Trie k a) (GTrie (Rep k) a)
     , Coercible (Trie k b) (GTrie (Rep k) b)
     , Coercible (Trie k c) (GTrie (Rep k) c)
     ) =>
     (k -> a -> b -> Maybe c) -> (Trie k a -> Trie k c) -> (Trie k b -> Trie k c) -> Trie k a -> Trie k b -> Trie k c
  trieMergeWithKey = genericTrieMergeWithKey

instance TrieKey k => Functor     (Trie k) where fmap     = fmapDefault
instance TrieKey k => Foldable    (Trie k) where foldMap  = foldMapDefault
instance TrieKey k => Traversable (Trie k) where traverse = trieITraverse

instance (Semigroup a, TrieKey k) => Monoid (Trie k a) where
  mappend = (<>)
  mempty  = trieEmpty

instance (Semigroup a, TrieKey k) => Semigroup (Trie k a) where
  (<>) = trieMergeWithKey (\_ a b -> Just (a <> b)) id id

instance TrieKey k => At   (Trie k a) where at = trieAt
instance TrieKey k => Ixed (Trie k a) where ix = ixAt


-- | Abstract type when generating tries from the 'Generic' representation of a key.
newtype GenericTrie k a = GenericTrie (GTrie (Rep k) a)

coerceMergeWithKey ::
  ( Coercible (f a) (g a)
  , Coercible (f b) (g b)
  , Coercible (f c) (g c)
  ) =>
  (z -> (f a -> f c) -> (f b -> f c) -> f a -> f b -> f c) ->

  z ->
  (g a -> g c) ->
  (g b -> g c) ->
  g a -> g b -> g c
coerceMergeWithKey = Data.Coerce.coerce
{-# INLINE coerceMergeWithKey #-}

-- Base instances

newtype instance Trie Int a = IntTrie (IntMap a)
instance TrieKey Int where
  trieAt k                    = iso (\(IntTrie t) -> t) IntTrie . at k
  trieNull (IntTrie x)        = IntMap.null x
  trieEmpty                   = IntTrie IntMap.empty
  trieITraverse f (IntTrie x) = fmap IntTrie (itraversed f x)
  trieMergeWithKey            = coerceMergeWithKey IntMap.mergeWithKey

newtype instance Trie Int8 a = Int8Trie (IntMap a)
instance TrieKey Int8 where
  trieAt k                     = iso (\(Int8Trie t) -> t) Int8Trie . at (fromEnum k)
  trieNull (Int8Trie x)        = IntMap.null x
  trieEmpty                    = Int8Trie IntMap.empty
  trieITraverse f (Int8Trie x) = fmap Int8Trie (reindexed (toEnum :: Int -> Int8) itraversed f x)
  trieMergeWithKey f           = coerceMergeWithKey IntMap.mergeWithKey (f . toEnum)

newtype instance Trie Int16 a = Int16Trie (IntMap a)
instance TrieKey Int16 where
  trieAt k                      = iso (\(Int16Trie t) -> t) Int16Trie . at (fromEnum k)
  trieNull (Int16Trie x)        = IntMap.null x
  trieEmpty                     = Int16Trie IntMap.empty
  trieITraverse f (Int16Trie x) = fmap Int16Trie (reindexed (toEnum :: Int -> Int16) itraversed f x)
  trieMergeWithKey f            = coerceMergeWithKey IntMap.mergeWithKey (f . toEnum)

newtype instance Trie Int32 a = Int32Trie (IntMap a)
instance TrieKey Int32 where
  trieAt k                      = iso (\(Int32Trie t) -> t) Int32Trie . at (fromEnum k)
  trieNull (Int32Trie x)        = IntMap.null x
  trieEmpty                     = Int32Trie IntMap.empty
  trieITraverse f (Int32Trie x) = fmap Int32Trie (reindexed (toEnum :: Int -> Int32) itraversed f x)
  trieMergeWithKey f            = coerceMergeWithKey IntMap.mergeWithKey (f . toEnum)

newtype instance Trie Int64 a = Int64Trie (Map Int64 a)
instance TrieKey Int64 where
  trieAt k                      = iso (\(Int64Trie t) -> t) Int64Trie . at k
  trieNull (Int64Trie x)        = Map.null x
  trieEmpty                     = Int64Trie Map.empty
  trieITraverse f (Int64Trie x) = fmap Int64Trie (itraversed f x)
  trieMergeWithKey              = coerceMergeWithKey Map.mergeWithKey

newtype instance Trie Word8 a = Word8Trie (IntMap a)
instance TrieKey Word8 where
  trieAt k                      = iso (\(Word8Trie t) -> t) Word8Trie . at (fromEnum k)
  trieNull (Word8Trie x)        = IntMap.null x
  trieEmpty                     = Word8Trie IntMap.empty
  trieITraverse f (Word8Trie x) = fmap Word8Trie (reindexed (toEnum :: Int -> Word8) itraversed f x)
  trieMergeWithKey f            = coerceMergeWithKey IntMap.mergeWithKey (f . toEnum)

newtype instance Trie Word16 a = Word16Trie (IntMap a)
instance TrieKey Word16 where
  trieAt k                       = iso (\(Word16Trie t) -> t) Word16Trie . at (fromEnum k)
  trieNull (Word16Trie x)        = IntMap.null x
  trieEmpty                      = Word16Trie IntMap.empty
  trieITraverse f (Word16Trie x) = fmap Word16Trie (reindexed (toEnum :: Int -> Word16) itraversed f x)
  trieMergeWithKey f             = coerceMergeWithKey IntMap.mergeWithKey (f . toEnum)

newtype instance Trie Word32 a = Word32Trie (Map Word32 a)
instance TrieKey Word32 where
  trieAt k                       = iso (\(Word32Trie t) -> t) Word32Trie . at k
  trieNull (Word32Trie x)        = Map.null x
  trieEmpty                      = Word32Trie Map.empty
  trieITraverse f (Word32Trie x) = fmap Word32Trie (itraversed f x)
  trieMergeWithKey               = coerceMergeWithKey Map.mergeWithKey

newtype instance Trie Word64 a = Word64Trie (Map Word64 a)
instance TrieKey Word64 where
  trieAt k                       = iso (\(Word64Trie t) -> t) Word64Trie . at k
  trieNull (Word64Trie x)        = Map.null x
  trieEmpty                      = Word64Trie Map.empty
  trieITraverse f (Word64Trie x) = fmap Word64Trie (itraversed f x)
  trieMergeWithKey               = coerceMergeWithKey Map.mergeWithKey

newtype instance Trie Integer a = IntegerTrie (Map Integer a)
instance TrieKey Integer where
  trieAt k f (IntegerTrie x)      = fmap IntegerTrie (at k f x)
  trieNull (IntegerTrie x)        = Map.null x
  trieEmpty                       = IntegerTrie Map.empty
  trieITraverse f (IntegerTrie x) = fmap IntegerTrie (itraversed f x)
  trieMergeWithKey                = coerceMergeWithKey Map.mergeWithKey


newtype instance Trie Char a = CharTrie (IntMap a)
instance TrieKey Char where
  trieAt k f (CharTrie x)      = fmap CharTrie (at (ord k) f x)
  trieNull (CharTrie x)        = IntMap.null x
  trieEmpty                    = CharTrie IntMap.empty
  trieITraverse f (CharTrie x) = fmap CharTrie (reindexed chr itraversed f x)
  trieMergeWithKey f           = coerceMergeWithKey IntMap.mergeWithKey (f . chr)


newtype instance Trie Bool a = BoolTrie (GenericTrie Bool a)
instance TrieKey Bool where

instance TrieKey k => TrieKey (Maybe k)
newtype instance Trie (Maybe k) a = MaybeTrie (GenericTrie (Maybe k) a)

instance (TrieKey a, TrieKey b) => TrieKey (Either a b)
newtype instance Trie (Either a b) v = EitherTrie (GenericTrie (Either a b) v)

instance TrieKey ()
newtype instance Trie () v = Tuple0Trie (GenericTrie () v)

instance (TrieKey a, TrieKey b) => TrieKey (a,b)
newtype instance Trie (a,b) v = Tuple2Trie (GenericTrie (a,b) v)

instance (TrieKey a, TrieKey b, TrieKey c) => TrieKey (a,b,c)
newtype instance Trie (a,b,c) v = Tuple3Trie (GenericTrie (a,b,c) v)

instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d) => TrieKey (a,b,c,d)
newtype instance Trie (a,b,c,d) v = Tuple4Trie (GenericTrie (a,b,c,d) v)

instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d, TrieKey e) => TrieKey (a,b,c,d,e)
newtype instance Trie (a,b,c,d,e) v = Tuple5Trie (GenericTrie (a,b,c,d,e) v)

instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d, TrieKey e, TrieKey f) => TrieKey (a,b,c,d,e,f)
newtype instance Trie (a,b,c,d,e,f) v = Tuple6Trie (GenericTrie (a,b,c,d,e,f) v)

instance (TrieKey a, TrieKey b, TrieKey c, TrieKey d, TrieKey e, TrieKey f, TrieKey g) => TrieKey (a,b,c,d,e,f,g)
newtype instance Trie (a,b,c,d,e,f,g) v = Tuple7Trie (GenericTrie (a,b,c,d,e,f,g) v)

instance TrieKey Ordering
newtype instance Trie Ordering v = OrderingTrie (GenericTrie Ordering v)

instance TrieKey k => TrieKey [k]
newtype instance Trie [k] a = ListTrie (GenericTrie [k] a)


genericTrieNull ::
  forall k a.
  ( GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  ) =>
  Trie k a -> Bool
genericTrieNull = coerceWith (sym Coercion) (gtrieNull :: GTrie (Rep k) a -> Bool)


genericTrieEmpty ::
  forall k a.
  ( GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  ) =>
  Trie k a
genericTrieEmpty = coerceWith (sym Coercion) (gtrieEmpty :: GTrie (Rep k) a)


genericTrieMergeWithKey ::
  forall k a b c.
  ( GTrieKey (Rep k)
  , Generic k
  , Coercible (Trie k a) (GTrie (Rep k) a)
  , Coercible (Trie k b) (GTrie (Rep k) b)
  , Coercible (Trie k c) (GTrie (Rep k) c)
  ) =>
  (k -> a -> b -> Maybe c) -> (Trie k a -> Trie k c) -> (Trie k b -> Trie k c) -> Trie k a -> Trie k b -> Trie k c
genericTrieMergeWithKey f =
  case sym (Coercion :: Coercion (Trie k a) (GTrie (Rep k) a)) of
    Coercion ->
      case sym (Coercion :: Coercion (Trie k b) (GTrie (Rep k) b)) of
        Coercion ->
          case sym (Coercion :: Coercion (Trie k c) (GTrie (Rep k) c)) of
            Coercion ->
               coerceMergeWithKey gtrieMergeWithKey (f . GHC.Generics.to)


genericTrieAt ::
  ( Generic k
  , GTrieKey (Rep k)
  , Coercible (Trie k a) (GTrie (Rep k) a)
  ) =>
  k -> Lens' (Trie k a) (Maybe a)
genericTrieAt k = iso Data.Coerce.coerce (coerceWith (sym Coercion)) . gtrieAt (GHC.Generics.from k)


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
  gtrieITraverse :: IndexedTraversal (f ()) (GTrie f a) (GTrie f b) a b
  gtrieMergeWithKey ::
     (f () -> a -> b -> Maybe c) ->
     (GTrie f a -> GTrie f c) ->
     (GTrie f b -> GTrie f c) ->
     GTrie f a -> GTrie f b -> GTrie f c



mtrieIso :: Iso (GTrie (M1 i c f) a) (GTrie (M1 i c f) b) (GTrie f a) (GTrie f b)
mtrieIso = iso (\(MTrie p) -> p) MTrie
{-# INLINE mtrieIso #-}

instance GTrieKey f => GTrieKey (M1 i c f) where
  newtype GTrie (M1 i c f) a = MTrie (GTrie f a)
  gtrieAt (M1 k)             = mtrieIso . gtrieAt k
  gtrieNull (MTrie m)        = gtrieNull m
  gtrieEmpty                 = MTrie gtrieEmpty
  gtrieMergeWithKey f        = coerceMergeWithKey gtrieMergeWithKey (c f)
    where
    c :: (M1 i c f () -> z) -> f () -> z
    c = Data.Coerce.coerce

  gtrieITraverse             = mtrieIso . reindexed m1 gtrieITraverse
    where
    m1 :: f () -> M1 i c f ()
    m1 = M1


ktrieIso :: Iso (GTrie (K1 i k) a) (GTrie (K1 i k') b) (Trie k a) (Trie k' b)
ktrieIso = iso (\(KTrie p) -> p) KTrie
{-# INLINE ktrieIso #-}

instance TrieKey k => GTrieKey (K1 i k) where
  newtype GTrie (K1 i k) a = KTrie (Trie k a)
  gtrieAt (K1 k)           = ktrieIso . trieAt k
  gtrieNull (KTrie k)      = trieNull k
  gtrieEmpty               = KTrie trieEmpty
  gtrieMergeWithKey f      = coerceMergeWithKey trieMergeWithKey (c f)
    where
    c :: (K1 i k () -> z) -> k -> z
    c = Data.Coerce.coerce

  gtrieITraverse           = ktrieIso . reindexed k1 trieITraverse
    where
    k1 :: a -> K1 i a ()
    k1 = K1


ptrieIso :: Iso (GTrie (f :*: g) a) (GTrie (f' :*: g') b) (GTrie f (GTrie g a)) (GTrie f' (GTrie g' b))
ptrieIso = iso (\(PTrie p) -> p) PTrie
{-# INLINE ptrieIso #-}

instance (GTrieKey f, GTrieKey g) => GTrieKey (f :*: g) where
  newtype GTrie (f :*: g) a = PTrie (GTrie f (GTrie g a))

  gtrieAt (i :*: j)   = ptrieIso
                      . gtrieAt i
                      . anon gtrieEmpty gtrieNull
                      . gtrieAt j

  gtrieEmpty          = PTrie gtrieEmpty
  gtrieNull (PTrie m) = gtrieNull m
  gtrieITraverse      = ptrieIso . icompose (:*:) gtrieITraverse gtrieITraverse

  gtrieMergeWithKey f g h (PTrie m) (PTrie n) =
    PTrie (gtrieMergeWithKey
              (\fk x y ->
                noEmpty
                  (gtrieMergeWithKey
                     (\gk -> f (fk :*: gk))
                     (wrap fk g)
                     (wrap fk h)
                     x y))
              (under ptrieIso g)
              (under ptrieIso h)
              m n)

    where
    noEmpty x
      | gtrieNull x = Nothing
      | otherwise   = Just x

    wrap k t x =
      let p = under ptrieIso t (set (gtrieAt k) (Just x) gtrieEmpty)
      in case view (gtrieAt k) p of
           Nothing -> gtrieEmpty
           Just x' -> x'


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
  gtrieMergeWithKey f g h (STrie m1 m2) (STrie n1 n2) =
    STrie (gtrieMergeWithKey (f . L1) (wrapL g) (wrapL h) m1 n1)
          (gtrieMergeWithKey (f . R1) (wrapR g) (wrapR h) m2 n2)
    where
    wrapL t x = case t (STrie x gtrieEmpty) of
                  STrie x' _ -> x'

    wrapR t x = case t (STrie gtrieEmpty x) of
                  STrie _ x' -> x'

  gtrieITraverse f (STrie x y) = STrie <$> reindexed l1 gtrieITraverse f x
                                       <*> reindexed r1 gtrieITraverse f y
    where
    l1 :: f () -> (f :+: g) ()
    l1 = L1
    r1 :: g () -> (f :+: g) ()
    r1 = R1


utrieIso :: Iso (GTrie U1 a) (GTrie U1 b) (Maybe a) (Maybe b)
utrieIso = iso (\(UTrie x) -> x) UTrie

u1 :: U1 ()
u1 = U1

instance GTrieKey U1 where
  newtype GTrie U1 a  = UTrie (Maybe a)
  gtrieAt _           = utrieIso
  gtrieEmpty          = UTrie Nothing
  gtrieNull (UTrie u) = isNothing u
  gtrieITraverse      = utrieIso . traverse . flip indexed u1

  gtrieMergeWithKey _ _ _ (UTrie Nothing) (UTrie Nothing) = UTrie Nothing
  gtrieMergeWithKey f _ _ (UTrie (Just x)) (UTrie (Just y)) = UTrie (f u1 x y)
  gtrieMergeWithKey _ g _ x                (UTrie Nothing) = g x
  gtrieMergeWithKey _ _ h (UTrie Nothing)  y               = h y


instance GTrieKey V1 where
  data GTrie V1 a             = VTrie
  gtrieAt k _ _               = k `seq` error "GTrieKey.V1: gtrieAt"
  gtrieEmpty                  = VTrie
  gtrieNull _                 = True
  gtrieITraverse _ _          = pure VTrie
  gtrieMergeWithKey _ _ _ _ _ = VTrie


instance TrieKey k => FunctorWithIndex     k (Trie k) where
instance TrieKey k => FoldableWithIndex    k (Trie k) where
instance TrieKey k => TraversableWithIndex k (Trie k) where
  itraverse  = trieITraverse . Indexed
  itraversed = trieITraverse

instance (Eq a, TrieKey k) => Eq (Trie k a) where
  x == y = Foldable.and (trieMergeWithKey merge (fmap (const False)) (fmap (const False)) x y)
     where
     merge _ a b
       | a == b    = Nothing
       | otherwise = Just False
