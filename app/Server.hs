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

type AppEnv m e
  = ( Provides String e
    , Provides (IO T.UTCTime) e
    , Embedded T.UTCTime e m
    , ProvidesLabel "derp" String e
    , ProvidesF Maybe Int e
    , ProvidesF [] Int e
    , ProvidesF IO Int e
    )

server :: AppEnv Handler e => ServerT API (ReaderT e Handler)
server = handler someDependency

api :: Proxy API
api = Proxy

app :: AppEnv Handler e => e -> Application
app s = serve api $ hoistServer api (nt s) server
  where nt s x = runReaderT x s
