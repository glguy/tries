{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
-- | This hideous module lets us avoid dealing with the fact that
-- various functions haven't always been exported from the standard
-- Prelude.
module Prelude
  ( module Prel
#if !MIN_VERSION_base(4,18,0)
  , Applicative (..)
#endif
#if !MIN_VERSION_base(4,10,0)
  , liftA2
#endif
#if !MIN_VERSION_base(4,8,0)
  , Foldable (foldMap)
  , Traversable (traverse)
  , Word
  , (<$>)
#endif
  )
  where

import "base" Prelude as Prel
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (Applicative (..))
#endif

#if !MIN_VERSION_base(4,10,0)
import Control.Applicative (liftA2)
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable (..))
import Data.Traversable (Traversable (..))
import Data.Word (Word)
import Control.Applicative ((<$>))
#endif
