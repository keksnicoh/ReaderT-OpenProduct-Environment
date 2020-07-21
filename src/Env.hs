{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeInType,
  TypeOperators, FlexibleInstances, GADTs, ScopedTypeVariables,
  AllowAmbiguousTypes, FlexibleContexts, TypeApplications,
  RankNTypes, ConstraintKinds, UndecidableInstances,
  FunctionalDependencies #-}

module Env
  ( nil
  , (#:)
  , Provides(..)
  , ProvidesF(..)
  , ProvidesEffect(..)
  , ProvidesLabel(..)
  , provide
  , provideF
  , effect
  , label
  , Labeled(..)
  , Embedded
  )
where

import           Data.Kind                      ( Type )
import qualified GHC.TypeLits                  as TL
import qualified Fcf
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

-- # public api

-- | provides a value by type
class Provides f e where
  provideFrom :: e -> f

-- | todo: ProvidesF
class ProvidesF f t e where
  provideFromF :: e -> f t

-- | provides a effectfull computation where the effect is determined by the environment
class ProvidesEffect f e m | e -> m where
  effectFrom :: e -> m f

-- | provides a labeled value by type
class ProvidesLabel l t e where
  labelFrom :: e -> t

type Embedded a e m = ProvidesEffect a e (ReaderT e m)
newtype Labeled (l :: TL.Symbol) (t :: Type) = Labeled { runLabeled :: t }
  deriving (Show, Eq)

provide :: forall t e m . (MonadReader e m, Provides t e) => m t
provide = asks $ provideFrom @t

provideF :: forall f t e m . (MonadReader e m, ProvidesF f t e) => m (f t)
provideF = asks $ provideFromF @f @t

effect :: forall t e m . (MonadReader e m, ProvidesEffect t e m) => m t
effect = join . asks $ effectFrom @t

label
  :: forall l t e m
   . (TL.KnownSymbol l, MonadReader e m, ProvidesLabel l t e)
  => m t
label = asks $ labelFrom @l

-- we use a phaontom type parameter to embed to environment into the
-- application stack
data Env (m :: Type -> Type) (ts :: [k]) where
  Env ::V.Vector Any -> Env m ts

nil :: Env m '[]
nil = Env V.empty

(#:) :: t -> Env m ts -> Env m (t ': ts)
ft #: (Env v) = Env $ V.cons (Any ft) v
infixr 5 #:

-- | when the underlying hlist contains a certain type, the environment provides the type as well
instance ProvidesF Identity t (Env m ts) => Provides t (Env m ts) where
  provideFrom = runIdentity . provideFromF

instance {-# Overlapping #-} Extractable t ts => ProvidesF [] t (Env m ts) where
  provideFromF = extract

instance {-# Overlapping #-} KnownMaybeNat (GetMaybeIndex t ts) => ProvidesF Maybe t (Env m ts) where
  provideFromF = getOpt @t

instance (Applicative f, Contains t ts) => ProvidesF f t (Env m ts) where
  provideFromF = pure . get @t

-- | embed a certain effectful computation if it is contained in the underlying hlist
instance Contains (m t) ts => ProvidesEffect t (Env m ts) (ReaderT (Env m ts) m) where
  effectFrom = ReaderT . const . get

-- | when the underlying hlist contains a certaing Labeled, the environment provides the value as well
instance Provides (Labeled l t) (Env m ts) => ProvidesLabel l t (Env m ts) where
  labelFrom = runLabeled . provideFrom @(Labeled l t)

-- # internal api ---------------------------------------------------------------------------------

data Any where
  Any ::t -> Any
unAny :: Any -> p
unAny (Any a) = unsafeCoerce a

get :: forall t ts m . Contains t ts => Env m ts -> t
get (Env v) = unAny $ V.unsafeIndex v $ indexByType @t @ts

getOpt
  :: forall t ts m . KnownMaybeNat (GetMaybeIndex t ts) => Env m ts -> Maybe t
getOpt (Env v) = unAny . V.unsafeIndex v <$> maybeIndexByType @t @ts

extract :: forall t ts m . Extractable t ts => Env m ts -> [t]
extract (Env v) = map (unAny . V.unsafeIndex v) (findIndexList @t @ts)

headHV :: forall t ts m . Env m (t ': ts) -> t
headHV = get @t

tailHV :: forall t ts m . Env m ts -> Env m (Eval (Tail ts))
tailHV (Env v) = Env (V.unsafeTail v)

-- # types
type GetIndex (t :: k) (ts :: [k])
  = Fcf.Eval
      ((Fcf.=<<) (Fcf.FromMaybe Fcf.Stuck) (Fcf.FindIndex (Fcf.TyEq t) ts))
type Extractable t ts = KnownListNat (Fcf.Eval (FindIndexList t ts))
type GetMaybeIndex (t :: k) (ts :: [k]) = Eval (FindIndex (TyEq t) ts)
type Contains t ts = TL.KnownNat (GetIndex t ts)

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
data Drop :: TL.Nat -> [a] -> Fcf.Exp [a]
type instance Fcf.Eval (Drop n as) = Drop_ n as

data Tail :: [a] -> Exp [a]
type instance Eval (Tail as) = Eval (Drop 1 as)

data FindIndexList :: a -> [a] -> Fcf.Exp [TL.Nat]
type instance Fcf.Eval (FindIndexList n as) = FindIndexList_ 0 n as

-- # runtime representations

indexByType :: forall t ts . TL.KnownNat (GetIndex t ts) => Int
indexByType = fromIntegral . TL.natVal $ Proxy @(GetIndex t ts)

maybeIndexByType
  :: forall t ts . KnownMaybeNat (GetMaybeIndex t ts) => Maybe Int
maybeIndexByType = fromIntegral <$> maybeNatVal (Proxy @(GetMaybeIndex t ts))

findIndexList
  :: forall t ts . KnownListNat (Fcf.Eval (FindIndexList t ts)) => [Int]
findIndexList =
  fromIntegral <$> listNatVal (Proxy @(Fcf.Eval (FindIndexList t ts)))

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
instance Show (Env m '[]) where
  show _ = "Nil"
instance (Show a, Show (Env m ts)) => Show (Env m (a ': ts)) where
  show hv = show (get @a hv) ++ " :# " ++ show (tailHV hv)

instance Eq (Env m '[]) where
  _ == _ = True
instance (Eq t, Eq (Env m ts)) => Eq (Env m (t ': ts)) where
  l1 == l2 = headHV l1 == headHV l2 && (tailHV l1 == tailHV l2)

instance Ord (Env m '[]) where
  compare _ _ = EQ
instance (Ord t, Ord (Env m ts)) => Ord (Env m (t ': ts)) where
  compare l1 l2 = case compare (headHV l1) (headHV l2) of
    EQ -> compare (tailHV l1) (tailHV l2)
    r  -> r
