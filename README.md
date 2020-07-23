# ReaderT-OpenProduct-Environment

Embedded ReaderT with OpenProduct environment allows to define static dependency tree having dynamic environment.
All factors of the product may be accessed thru generic type classes thus it's not required to define individual type classes for each field. 
One goal of this approach is to reduce the number of typeclasses describing the environment.
The pattern is independent of Frameworks, however, in this repo, Servant is used to lift the examples into a "real" application.

Use `Provides` constraint to access pure values while effectfull values `m e` can ebmedded via `Embedded` constraint into `ReaderT e m` context.

Dependency injection is done explicitly by function arguments. However, approaches like tagless final etc are possible.

This is a playground repository.

- Typelevel computations are based on [S. Maguire - Thinking with Types][1]
- [Embedded ReaderT Pattern][2]
- Example Project using embedded ReaderT: [toroise-service][3]

## Example

```haskell
type SomeDependency m = Int -> m String
handler
  :: ( MonadReader e m
     , Provides String e       -- e contains at least one String
     , Embedded T.UTCTime e m  -- e contains UTCTime embeddable into m
     , Labeled "derp" String e -- e contains at list one (Label "derp" String)
     )
  => SomeDependency m -- dependency injection
  -- ... n dependencies
  -> Maybe Int        -- args
  -> m [String]
handler someDependency argument = do
  value             <- provide @String
  currentTime       <- embedded @T.UTCTime
  someLabeledString <- labeled @"derp"
  anotherString     <- someDependency argument

  return [show argument, value, show currentTime, someLabeledString, anotherString]
```

`Provide == ProvideF Identity` and `Embedded == EmbeddedF Identity`.
There are `*F f` instances for all `Applicative f`

```haskell
handler
  :: ( MonadReader e m
     , MonadIO m
     , ProvidesF Maybe Int e -- optional
     , ProvidesF [] Int e    -- many values
     , ProvidesF IO Int e    -- kind of useless.. but possible
     , EffectF Maybe String e m
     )
  => m String
handler = do
  maybe       <- provideF @Maybe @Int         -- provide maybe
  list        <- provideF @[] @Int            -- provide list
  bla         <- provideF @IO @Int >>= liftIO -- applicatives in general are working, thus IO
  maybeString <- embeddedF @Maybe @String
  return $ show maybe ++ show list ++ show bla ++ show maybeString
```

As effects may embedd into the stack, the `MonadIO` constraint is not required in many cases, thus any pure monad can be used within specs / mocks.

```haskell
embeddedTest
  :: (MonadReader e m, EmbeddedF Maybe String e m, EmbeddedF [] Int e m)
  => m [String]
embeddedTest = do
  a <- fromMaybe "default" <$> embeddedF @Maybe @String
  b <- embeddedF @[] @Int
  return $ [a] <> (show <$> b)

-- specs

main :: IO ()
main = hspec $ do
  describe "embeddedTest" $ do
    it "should work without any effect in the environment"
      $ let env    = "test" #: nil
            result = runIdentity $ runReaderT embeddedTest env
        in  result `shouldBe` ["default"]
    it "should work without any effect in the environment"
      $ let env =
              pure @Identity "foo"
                #: pure @Identity @Int 1
                #: pure @Identity @Int 2
                #: nil
            result = runIdentity $ runReaderT embeddedTest env
        in  result `shouldBe` ["foo", "1", "2"]
```

while in a real application receiving the current time might be setup as follows

```bash
let
  env =
    "foo"
      #: liftIO @Handler T.getCurrentTime
      #: Label @"derp" "some-string"
      #: nil
```

we note that embedded environmental effects distinguish to argument dependency injection by the fact that they are contained in the underlying monad and thus do not have access to the environment.

```bash
stack test
```

## Summary

- Environment provides pure values (`ProvidesF`)
- Environment contains effects (`EffectF`) which are embeddable into application Monad but do not have access to the environment.
- Explicit static dependency injection via function arguments
- No custom type classes required

[1]: https://leanpub.com/thinking-with-types
[2]: https://stackoverflow.com/questions/61780295/readert-design-pattern-parametrize-the-environment
[3]: https://github.com/keksnicoh/tortoise-service
