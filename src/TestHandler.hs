{-# LANGUAGE DataKinds, TypeApplications, FlexibleContexts #-}

module TestHandler where

import qualified Data.Time                     as T
import           Control.Monad.Reader           ( liftIO
                                                , join
                                                , MonadIO
                                                , MonadReader
                                                )
import           Env
import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )
import           Data.Maybe                     ( fromMaybe )

-- we need some MonadReader with an environment
-- 1. providing a String
-- 2. providing some effectfull UTCTime
-- 3. providing a labeled String
handler
  :: ( MonadReader e m
     , Provides String e
     , Embedded T.UTCTime e m
   --  , EmbeddedF Maybe T.UTCTime e m
     , Labeled "derp" String e
     )
  => SomeDependency m -- dependency injection
  -- ... n dependencies
  -> Maybe Int        -- args
  -> m String
handler someDependency a = do
  val               <- provide
  currentTime       <- embedded @T.UTCTime
  --currentTimeMaybe       <- embeddedF @Maybe @T.UTCTime
  --currentTimeMaybe       <- embeddedF @[] @T.UTCTime
  someLabeledString <- labeled @"derp"
  anotherString     <- someDependency

  let myListToBeRendered =
        [val, show a, show currentTime, someLabeledString, anotherString]

  return $ show myListToBeRendered

-- type alias provides some context.
type SomeDependency m = m String
someDependency
  :: ( MonadReader e m
     , MonadIO m
     , ProvidesF Maybe Int e -- optional
     , ProvidesF [] Int e    -- many values
     , ProvidesF IO Int e
     )    -- kind of useless, but possible
  => SomeDependency m
someDependency = do
  maybe <- provideF @Maybe @Int         -- provide maybe
  list  <- provideF @[] @Int            -- provide list
  bla   <- provideF @IO @Int >>= liftIO -- applicatives in general are working like Identity->f
  return $ show maybe ++ show list ++ show bla

-- | labels can be fetched in functorial context
labelTest
  :: (MonadReader e m, LabeledF Maybe "a" String e, LabeledF [] "b" String e)
  => m [String]
labelTest = do
  a <- fromMaybe "default" <$> labeledF @Maybe @"a" @String
  b <- labeledF @[] @"b" @String
  return $ [a] <> b

-- | labels can be fetched in functorial context
embeddedTest
  :: (MonadReader e m, EmbeddedF Maybe String e m, EmbeddedF [] Int e m)
  => m [String]
embeddedTest = do
  a <- fromMaybe "default" <$> embeddedF @Maybe @String
  b <- embeddedF @[] @Int
  return $ [a] <> (show <$> b)
