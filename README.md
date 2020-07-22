# ReaderT-OpenProduct-Environment

Embedded ReaderT with OpenProduct environment allows to define static dependency tree having dynamic environment.
Instead of a simple datatype, an OpenProduct is used.
All factors of the product may be accessed by generic type class instances thus it's not required to define individual type classes for each field. The pattern is independent of Frameworks, however, in this repo, Servant is used to lift the examples into a "real" application.

Use `Provides` constraint to access pure values while effectfull values `m e` can ebmedded via `Embedded` constraint in `MonadReader e m` context.

Most of the ideas in this repository are inspired by [S. Maguire - Thinking with Types][1]

## Example

```haskell
handler
  :: ( MonadReader e m
     , Provides String e       -- e contains at least one String
     , Embedded T.UTCTime e m  -- e contains UTCTime embeddable into m
     , Labeled "derp" String e -- e contains at list one (Label "derp" String)
     )
  => SomeDependency m -- dependency injection
  -- ... n dependencies
  -> Maybe Int        -- args
  -> m String
handler someDependency a = do
  val               <- provide
  currentTime       <- embedded @T.UTCTime
  someLabeledString <- labeled @"derp"
  anotherString     <- someDependency
  let myListToBeRendered =
        [val, show a, show currentTime, someLabeledString, anotherString]

  return $ show myListToBeRendered
```

`Provide == ProvideF Identity` and `Embedded == EmbeddedF Identity`.
There are `*F f` instances for all `Applicative f`

```haskell
-- type alias provides some context.
type SomeDependency m = m String
someDependency
  :: ( MonadReader e m
     , MonadIO m
     , ProvidesF Maybe Int e -- optional
     , ProvidesF [] Int e    -- many values
     , ProvidesF IO Int e    -- kind of useless.. but possible
     , EffectF Maybe String e m
     )
  => SomeDependency m
someDependency = do
  maybe       <- provideF @Maybe @Int         -- provide maybe
  list        <- provideF @[] @Int            -- provide list
  bla         <- provideF @IO @Int >>= liftIO -- applicatives in general are working, thus IO
  maybeString <- embeddedF @Maybe @String
  return $ show maybe ++ show list ++ show bla ++ show maybeString
```

As effects may embedd into the stack, the `MonadIO` constraint is not required in many cases, thus any pure monad can be used within specs / mocks.

```haskell
main :: IO ()
main = hspec $ do
  describe "handler" $ do
    it "should work with underlying Identity monad"
      $ let
          env = 
            "depf"
              #: pure @Identity time
              #: Label @"derp" "yo"
              #: nil
          someDependency = return "hi"
          testHandler    = handler someDependency
          reader         = testHandler (Just 5)
          result         = runIdentity $ runReaderT reader env
        in
          result
            `shouldBe` "[\"depf\",\"Just 5\",\"2020-07-21 09:49:39.399779 UTC\",\"yo\",\"hi\"]"
```

while in a real application receiving the current time might be setup as follows

```bash
let
  env =
    "foo"
      #: T.getCurrentTime
      #: liftIO @Handler T.getCurrentTime
      #: Label @"derp" "some-string"
      #: nil
```

we note that effectfull environment distinguishes to argument dependency injection by the fact that values are only contained in the underlying monad and thus do not have access to the environment.

```bash
stack test
```

## Summary

- Environment provides pure values (`ProvidesF`)
- Environment contains effects (`EffectF`) which are embeddable into application Monad but do not have access to the environment.
- Explicit static dependency injection via function arguments
- No custom type classes required

[1]: https://leanpub.com/thinking-with-types