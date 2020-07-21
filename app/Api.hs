{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Servant.API
import           Data.Time

type TestHandlerEndpoint = GET
type API = QueryParam "bla" Int :> Get '[JSON] String