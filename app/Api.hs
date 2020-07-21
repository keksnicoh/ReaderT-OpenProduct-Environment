{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Servant.API
import           Data.Time

type API = QueryParam "bla" Int :> Get '[JSON] String