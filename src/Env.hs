{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Env

Provides Type Classes and convinience methods to interact with an environment
defined by HList.

-}
module Env
  ( nil
  , (#:)
  , Provides(..)
  , ProvidesF(..)
  , EmbeddedF(..)
  , provide
  , provideF
  , embedded
  , embeddedF
  , labeled
  , labeledF
  , Labeled(..)
  , LabeledF(..)
  , Label(..)
  , Embedded
  )
where

import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), asks, join)
import           Data.Kind              (Type)
import qualified GHC.TypeLits           as TL
import           HList                  (Get (..), HList, nil, ( #: ))

type Provides a e = ProvidesF Identity a e
type Embedded a e m = EmbeddedF Identity a e m
type Labeled l a e = Provides (Label l a) e
type LabeledF f l a e = ProvidesF f (Label l a) e

-- | provides an effectfull computation
class ProvidesF f a e where
  provideFromF :: e -> f a

-- | provides an embedded effectfull computation
class EmbeddedF f a e m where
  embeddedFromF :: e -> m (f a)

instance Get f t ts => ProvidesF f t (HList ts) where
  provideFromF = getF

instance (Traversable t, Applicative m, Get t (m a) ts)
  => EmbeddedF t a (HList ts) (ReaderT (HList ts) m) where
  embeddedFromF = ReaderT . const . sequenceA . getF

newtype Label (l :: TL.Symbol) (t :: Type)
  = Label { runLabel :: t }
  deriving (Show, Eq)

{-|
  provides a value from environment
  @
    f :: (MonadReader e m, Provide String e) -> m String
    f = do
      string <- provide @String
      return string
  @
-}
provide :: forall t e m . (MonadReader e m, Provides t e) => m t
provide = asks $ runIdentity . provideFromF @Identity @t

{-|
  provides a boxed value from environment
  @
    f :: (MonadReader e m, ProvideF @[] String e) -> m [String]
    f = do
      strings <- provideF @String
      return strings
  @
-}
provideF :: forall f t e m . (MonadReader e m, ProvidesF f t e) => m (f t)
provideF = asks $ provideFromF @f @t

{-|
  performs an `m a` from environment within `MonadReader e m` by lifting it into ReaderT.

  @
    f :: (MonadReader e m, Embedded String e m) -> m String
    f = do
      string <- embedded @String
      return string
  @
-}
embedded :: forall a e m . (MonadReader e m, EmbeddedF Identity a e m) => m a
embedded = runIdentity <$> embeddedF @Identity @a

{-|
  receives an applicative effect within traversable context from environment

  @
    f :: (MonadReader e m, EmbeddedF [] String e m) -> m [String]
    f = do
      strings <- embeddedF @[] @String
      return strings
  @
-}
embeddedF :: forall t a e m . (MonadReader e m, EmbeddedF t a e m) => m (t a)
embeddedF = join . asks $ embeddedFromF

{-|
  receives label from environment

  @
    f :: (MonadReader e m, Labeled "a" String e) -> m String
    f = do
      label <- labeled  @"a" @String
      return label
  @
-}
labeled :: forall l a e m . (MonadReader e m, Labeled l a e) => m a
labeled = runLabel <$> provide @(Label l a)

{-|
  receives label within functorial context from environment

  @
    f :: (MonadReader e m, LabeledF [] "a" String e) -> m [String]
    f = do
      labels <- labeledF @[] @"a" @String
      return labels
  @
-}
labeledF
  :: forall f l a e m
   . (Functor f, MonadReader e m, LabeledF f l a e)
  => m (f a)
labeledF = fmap runLabel <$> provideF @f @(Label l a)
