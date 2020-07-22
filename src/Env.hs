{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeInType,
  TypeOperators, FlexibleInstances, GADTs, ScopedTypeVariables,
  AllowAmbiguousTypes, FlexibleContexts, TypeApplications,
  RankNTypes, ConstraintKinds, UndecidableInstances,
  MultiParamTypeClasses #-}

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
import           Fcf                     hiding ( Tail )
import           Data.Proxy                     ( Proxy(..) )
import           Unsafe.Coerce                  ( unsafeCoerce )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , asks
                                                , join
                                                , MonadReader
                                                )
import qualified Data.Vector                   as V
import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )

-- # basic datatype

data Env (ts :: [k]) where
  Env ::V.Vector Any -> Env ts

nil :: Env '[]
nil = Env V.empty

(#:) :: t -> Env ts -> Env (t ': ts)
ft #: (Env v) = Env $ V.cons (Any ft) v
infixr 5 #:

-- # public api

type Provides a e = ProvidesF Identity a e
type Embedded a e m = EmbeddedF Identity a e m
type Labeled l a e = Provides (Label l a) e

-- | provides an effectfull computation
class ProvidesF f a e where
  provideFromF :: e -> f a

-- | provides an embedded effectfull computation
class EmbeddedF f a e m where
  embeddedFromF :: e -> m (f a)

instance Get f t ts => ProvidesF f t (Env ts) where
  provideFromF = getF

instance (Traversable t, Applicative m, Get t (m a) ts)
  => EmbeddedF t a (Env ts) (ReaderT (Env ts) m) where
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

-- # internal api ---------------------------------------------------------------------------------

class Get f t ts where
  getF :: Env ts -> f t

instance {-# Overlapping #-} HasMaybeIndexOf t ts => Get Maybe t ts where
  getF (Env v) = unAny . V.unsafeIndex v <$> maybeIndexOf @t @ts

instance {-# Overlapping #-} HasListIndices t ts => Get [] t ts where
  getF (Env v) = map (unAny . V.unsafeIndex v) (getListIndices @t @ts)

instance (Applicative f, HasIndexOf t ts) => Get f t ts where
  getF (Env v) = pure . unAny . V.unsafeIndex v $ indexOf @t @ts

data Any where
  Any ::t -> Any
unAny :: Any -> p
unAny (Any a) = unsafeCoerce a

get :: forall t ts . Get Identity t ts => Env ts -> t
get = runIdentity . getF

headHV :: forall t ts . Env (t ': ts) -> t
headHV = get

tailHV :: forall t ts m . Env ts -> Env (Eval (Tail ts))
tailHV (Env v) = Env (V.unsafeTail v)

indexOf :: forall t ts . HasIndexOf t ts => Int
indexOf = fromIntegral . TL.natVal $ Proxy @(IndexOf t ts)

maybeIndexOf :: forall t ts . HasMaybeIndexOf t ts => Maybe Int
maybeIndexOf = fromIntegral <$> maybeNatVal (Proxy @(MaybeIndexOf t ts))

getListIndices :: forall t ts . HasListIndices t ts => [Int]
getListIndices = fromIntegral <$> listNatVal (Proxy @(ListIndices t ts))

-- # types
type HasIndexOf t ts = TL.KnownNat (IndexOf t ts)
type HasMaybeIndexOf t ts = KnownMaybeNat (MaybeIndexOf t ts)
type HasListIndices t ts = KnownListNat (ListIndices t ts)

type IndexOf (t :: k) (ts :: [k])
  = Eval (FromMaybe Stuck =<< FindIndex (TyEq t) ts)
type MaybeIndexOf (t :: k) (ts :: [k]) = Eval (FindIndex (TyEq t) ts)
type ListIndices (t :: k) (ts :: [k]) = Eval (FindIndexList t ts)

-- # type families
type family FindIndexList_ (i :: TL.Nat) (x :: a) (xs :: [a]) :: [TL.Nat] where
  FindIndexList_ _ _ '[]       = '[]
  FindIndexList_ i t (t ': xs) = i ': FindIndexList_ ((TL.+) i 1) t xs
  FindIndexList_ i o (t ': xs) =      FindIndexList_ ((TL.+) i 1) o xs

type family Drop_ (n :: TL.Nat) (xs :: [a]) :: [a] where
  Drop_ _ '[]       = '[]
  Drop_ 0  as       = as
  Drop_ n (x ': xs) = Drop_ ((TL.-) n 1) xs

-- # FCF
data Drop :: TL.Nat -> [a] -> Exp [a]
type instance Eval (Drop n as) = Drop_ n as

data Tail :: [a] -> Exp [a]
type instance Eval (Tail as) = Eval (Drop 1 as)

data FindIndexList :: a -> [a] -> Exp [TL.Nat]
type instance Eval (FindIndexList n as) = FindIndexList_ 0 n as

-- type demotion

class KnownMaybeNat (v :: Maybe TL.Nat) where maybeNatVal :: Proxy v -> Maybe Integer
instance KnownMaybeNat Nothing where
  maybeNatVal _ = Nothing
instance TL.KnownNat n => KnownMaybeNat (Just n) where
  maybeNatVal x = Just $ TL.natVal (unJust x)
   where
    unJust :: Proxy (Just n) -> Proxy n
    unJust _ = Proxy

class KnownListNat (v :: [TL.Nat]) where listNatVal :: Proxy v -> [Integer]
instance KnownListNat '[] where
  listNatVal _ = []
instance (TL.KnownNat x, KnownListNat xs) => KnownListNat (x ': xs) where
  listNatVal l = TL.natVal (headP l) : listNatVal (tailP l)
   where
    headP :: Proxy (n ': ns) -> Proxy n
    headP _ = Proxy
    tailP :: Proxy (n ': ns) -> Proxy ns
    tailP _ = Proxy

-- instances

instance Show (Env '[]) where
  show _ = "Nil"
instance (Show a, Show (Env ts)) => Show (Env (a ': ts)) where
  show hv = show (get @a hv) ++ " :# " ++ show (tailHV hv)

instance Eq (Env '[]) where
  _ == _ = True
instance (Eq t, Eq (Env ts)) => Eq (Env (t ': ts)) where
  l1 == l2 = headHV l1 == headHV l2 && (tailHV l1 == tailHV l2)

instance Ord (Env '[]) where
  compare _ _ = EQ
instance (Ord t, Ord (Env ts)) => Ord (Env (t ': ts)) where
  compare l1 l2 = case compare (headHV l1) (headHV l2) of
    EQ -> compare (tailHV l1) (tailHV l2)
    r  -> r
