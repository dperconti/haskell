# 14 Testing

There are generally four recognized levels of tests:

1. **Unit testing** tests small units of code, generally on function level, or in object-oriented programming environments on class level.
2. **Integration testing** verifies the interfaces between components against design specifications. It ensures that the units (tested in 1.) are wired up properly.
3. **Component interface** testing controls the data that is passed between units. The data is commonly logged. Unusual data values in an interface can help explain unexpected performance in the next unit.
4. **System testing** (aka. **end-to-end testing**) tests a completely integrated system to verify that the system meets its requirements.
A property-based testing framework runs the same test over and over with generated input.

## 14.1 Hspec
[Hspec](https://hspec.github.io/) is a Haskell testing framework. In order to work with it, the dependency hspec must be added or it can be installed manually.

```shell
cabal install hspec
```

```haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
```

## 14.2 QuickCheck
[QuickCheck](https://hackage.haskell.org/package/QuickCheck) was the first library to offer what is today called property testing.

```shell
cabal install QuickCheck
```

Sample snippet which uses QuickCheck in combination with hspec. QuickCheck itself does not provide the describe and it methods.

```haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
```

Another workflow is calling `quickCheck (fn :: signature)`.

QuickCheck validates the property by plugging in random values and edge cases. These are generated in this manner: `sample (arbitrary :: Gen Int)`.

Generators select values from a list, e.g. lowercase characters.

```haskell
genChar :: Gen Char
genChar = elements ['a'..'z']
```

This generator is more sophisticated and capable of generating lists of _n_ elements of type `a`, where _n_) is randomly chosen within an upper and lower bound.

```haskell
arbitraryList :: (Arbitrary a) => (Int, Int) -> Gen [a]
arbitraryList = flip genList arbitrary

genList :: (Int, Int) -> Gen a -> Gen [a]
genList (minL, maxL) g = sized $ \n -> do
  k <- choose (minL, min (max minL n) maxL)
  sequence [ g | _ <- [1..k] ]
```

`CoArbitrary` is used when random functions need to be generated. Learn more about [CoArbitrary here](https://carlo-hamalainen.net/2018/01/30/quickchecks-coarbitrary-generate-random-functions/)