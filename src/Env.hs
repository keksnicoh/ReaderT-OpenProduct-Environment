{-# LANGUAGE DataKinds, PolyKinds, FlexibleInstances,
  ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts,
  TypeApplications, RankNTypes, ConstraintKinds,
  UndecidableInstances, MultiParamTypeClasses #-}

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
  , Labeled(..)
  , Label(..)
  , Embedded
  )
where

import           Data.Kind                      ( Type )
import qualified GHC.TypeLits                  as TL
import           Control.Monad.Reader           ( ReaderT(..)
                                                , asks
                                                , join
                                                , MonadReader
                                                )
import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )
import           HList

type Provides a e = ProvidesF Identity a e
type Embedded a e m = EmbeddedF Identity a e m
type Labeled l a e = Provides (Label l a) e

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

provide :: forall t e m . (MonadReader e m, Provides t e) => m t
provide = asks $ runIdentity . provideFromF @Identity @t

provideF :: forall f t e m . (MonadReader e m, ProvidesF f t e) => m (f t)
provideF = asks $ provideFromF @f @t

embedded :: forall a e m . (MonadReader e m, EmbeddedF Identity a e m) => m a
embedded = runIdentity <$> embeddedF @Identity @a

embeddedF :: forall t a e m . (MonadReader e m, EmbeddedF t a e m) => m (t a)
embeddedF = join . asks $ embeddedFromF

labeled :: forall l a e m . (MonadReader e m, Labeled l a e) => m a
labeled = runLabel <$> provide @(Label l a)
