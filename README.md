# ReaderT-OpenProduct-Environment

This is a playground repository. ReaderT pattern using open environment. The pattern allows to define static dependency tree having dynamic environment.
Instead of a simple adt, an open-product is used to represent the application environment.
All factors of the product can be accessed by generic type class instances thus it's not required to define individual type classes. The pattern is independent of frameworks, however, in this repo, Servant is used to lift the examples into a "real" application.

Use `Provides v e` constraint to access pure values `v` while environmental effects `m v` can be ebmedded via `Embedded v e m` constraint.

- Typelevel computations are based on [S. Maguire - Thinking with Types][1]
- [Embedded ReaderT Pattern][2]
- Example Project using embedded ReaderT: [toroise-service][3]

## Example

```haskell
type SomeDependency m = Int -> m String

handler
  :: ( MonadReader e m
     , Provides String e       -- environment e contains at least one String
     , Embedded T.UTCTime e m  -- environment e contains effectful T.UTCTime
     , Labeled "derp" String e -- environment e contains at least one (Label "derp" String)
     )
  => SomeDependency m -- dependency injection
  -- ... n dependencies
  -> Maybe Int        -- function argument
  -> m [String]       -- result
handler someDependency argument = do
  value             <- provide @String         -- access pure value
  currentTime       <- embedded @T.UTCTime     -- access effectfull value
  someLabeledString <- labeled @"derp"         -- access labeled value

  anotherString     <- someDependency 5        -- bind effect from injected function

  return [show argument, value, show currentTime, someLabeledString, anotherString]
```

The latter type classes are special cases of more general ones:

```
Provide = ProvideF Identity
Embedded = EmbeddedF Identity
```

Example of more general interaction with the environment

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

As environmental effects can be embedded into the stack, the constraints like `MonadIO` may not be required in most cases, thus any monad can be used within specs

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

while the runtime environment might be setup as follows

```bash
let
  env =
    "foo"
      #: liftIO @Handler T.getCurrentTime
      #: Label @"derp" "some-string"
      #: nil
```
note that embedded effects from the environment distinguishe to argument dependency injection by the fact that they are contained in the underlying monad and thus do not have access to the environment.

```bash
stack test
```

## Summary

- Environment provides pure values (`ProvidesF`)
- Environment contains monad values (`EffectF`) which are embeddable into application Monad but do not have access to the environment.
- Explicit static dependency injection via function arguments
- No custom type classes required

[1]: https://leanpub.com/thinking-with-types
[2]: https://stackoverflow.com/questions/61780295/readert-design-pattern-parametrize-the-environment
[3]: https://github.com/keksnicoh/tortoise-service
