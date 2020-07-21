{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified Network.Wai.Handler.Warp
import qualified Env
import qualified Server
import           Lib
import           Env
import qualified Data.Time                     as T
import           Control.Monad.Reader           ( ReaderT
                                                , MonadIO(liftIO)
                                                )
import           Servant.Server.Internal.Handler
                                                ( Handler )
import           TestHandler

main :: IO ()
main = Network.Wai.Handler.Warp.run 1337 (Server.app env)
 where
  env =
    "foo"
      #: T.getCurrentTime
      #: liftIO @Handler T.getCurrentTime
      #: Labeled @"derp" "ieks"
      #: Labeled @"bork" "the bork val"
      #: (123 :: Int) -- note that we can happily drop these
      #: (456 :: Int)
      #: nil
