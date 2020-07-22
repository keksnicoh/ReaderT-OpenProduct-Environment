{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import           Control.Monad.Reader
import           Servant
import           Api
import           Env
import qualified Data.ByteString.Lazy          as LBS
import           Network.Wai                    ( Application )
import           TestHandler
import qualified Data.Time                     as T
import Control.Monad.Identity (Identity)

-- must reflect all constraint of the dependency tree
type AppEnv m e
  = ( Provides String e
    , Provides (IO T.UTCTime) e
    , EmbeddedF Identity T.UTCTime e m
    , Labeled "derp" String e
    , ProvidesF Maybe Int e
    , ProvidesF [] Int e
    , ProvidesF IO Int e
    )

server :: AppEnv (ReaderT e Handler) e => ServerT API (ReaderT e Handler)
server = handler someDependency

api :: Proxy API
api = Proxy

app :: AppEnv (ReaderT e Handler) e => e -> Application
app s = serve api $ hoistServer api (nt s) server
  where nt s x = runReaderT x s