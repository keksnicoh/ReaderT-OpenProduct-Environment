{-# LANGUAGE DataKinds, TypeApplications #-}

import           Test.Hspec
import           TestHandler
import           Env
import           Control.Monad.Identity
import qualified Data.Time                     as T
import           Control.Monad.Reader           ( ReaderT(runReaderT) )

time :: T.UTCTime
time = read "2020-07-21 09:49:39.399779 UTC"

main :: IO ()
main = hspec $ do
  describe "handler" $ do
    it "should work with underlying Identity monad"
      $ let
          -- note: the dependency on the "bork" label does not exists due to the mocked someDependency
          env = "depf" #: pure @Identity time #: Labeled @"derp" "yo" #: nil
          someDependency = return "hi"
          testHandler    = handler someDependency
          reader         = testHandler (Just 5)
          result         = runIdentity $ runReaderT reader env
        in
          result
            `shouldBe` "[\"depf\",\"Just 5\",\"2020-07-21 09:49:39.399779 UTC\",\"yo\",\"hi\"]"
    it "should work with underlying IO monad" $ do
      let env = "depf" #: pure @IO time #: Labeled @"derp" "yo" #: nil
          someDependency = return "jo"
          testHandler    = handler someDependency
          reader         = testHandler Nothing
      result <- runReaderT reader env
      result
        `shouldBe` "[\"depf\",\"Nothing\",\"2020-07-21 09:49:39.399779 UTC\",\"yo\",\"jo\"]"

