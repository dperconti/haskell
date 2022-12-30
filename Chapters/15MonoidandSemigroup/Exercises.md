# Chapter 15 Exercises

## Exercise: Optional Monoid
Write the Monoid instance for our Maybe type, renamed to Optional:

```haskell
module Optional where

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x = x
  (<>) x Nada = x
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)
```


## Exercise: Madness
You may have seen mad libs before. The idea is to take a template of phrases, fill them in with blindly selected categories of words, and see if saying the final version is amusing.

Using a lightly edited example from the Wikipedia article on Mad Libs:

```text
"___________! he said ______ as he
 exclamation          adverb
jumped into his car ____ and drove
                    noun
off with his _________ wife."
             adjective
```

We can make this into a function, like the following:
... Formatting copy/pasta nightmare ...
Now, you’re going to refactor this code a bit! Rewrite it using mconcat:

```haskell

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  mconcat [e, "! he said ",
           adv, " as he jumped into his car ",
           noun, " and drove off with this ",
           adj, " wife."]
```

## Testing QuickCheck’s patience
Let us see an example of QuickCheck catching us out for having an invalid Monoid. We’re going to demonstrate why a Bool Monoid can’t have False as the identity, always returning the value False, and still be a valid Monoid. Associative, left identity, and right identity properties have been elided from the following example. Add them:

```haskell
import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools)
              ,(1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)

type BullMappend =
  Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)
```

## Exercise: Maybe another Monoid
Write a Monoid instance for a Maybe type that doesn’t require a Monoid for the contents. Reuse the Monoid law QuickCheck properties, and use them to validate the instance.
Don’t forget to write an Arbitrary instance for First'. We won’t always stub that out explicitly for you. We suggest learning how to use the frequency function from QuickCheck for the instance of First':

```haskell
import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x = x
  (<>) x Nada = x
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
  mappend = (<>)

newtype First' a =
  First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
  (<>) (First' a) (First' a') = First' (a <> a')

instance Monoid a => Monoid (First' a) where
  mempty  = First' Nada
  mappend = (<>)

firstMappend :: Monoid a
             => First' a
             -> First' a
             -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada)
                        ,(1, fmap Only arbitrary)]

firstGen :: Arbitrary a => Gen (First' a)
firstGen =  First' <$> arbitrary

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

```


## Semigroup exercises
Given a datatype, implement the Semigroup instance. Add Semigroup constraints to type variables where needed. Use the Semigroup class from base or write your own. When we use <>, we mean the infix mappend operation from the Semigroup type class.
Note We’re not always going to derive every instance you may want or need in the datatypes we provide for exercises. We expect you to know what you need and to take care of it yourself by this point.

```haskell
import Data.Semigroup as S
import Data.Monoid as M
import Test.QuickCheck

-- #1. Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- #2. Identity a
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity a') = Identity (a <> a')

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = Identity <$> arbitrary
-- identityGen = do
--   a <- arbitrary
--   return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityAssoc = Identity String -> Identity String
  -> Identity String -> Bool

-- #3. Two a b
data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc = Two String (Sum Int) -> Two String (Sum Int)
  -> Two String (Sum Int) -> Bool

-- #4. Three a b c
data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') =
         Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c)
  => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = threeGen

type ThreeAssoc = Three String (Sum Int) (Product Int)
   -> Three String (Sum Int) (Product Int)
   -> Three String (Sum Int) (Product Int)
   -> Bool

-- #5. Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
  => Semigroup (Four a b c d) where
    (<>) (Four a b c d) (Four a' b' c' d')
       = Four (a <> a') (b <> b') (c <> c') (d <> d')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
    arbitrary = fourGen

type FourAssoc =
      Four String (Sum Int) (Product Int) String
   -> Four String (Sum Int) (Product Int) String
   -> Four String (Sum Int) (Product Int) String
   -> Bool

-- #6. BoolConj
newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) =
        BoolConj True
  (<>) (BoolConj _) (BoolConj _) =
        BoolConj False

boolConjGen :: Gen BoolConj
boolConjGen = BoolConj <$> arbitrary
-- boolConjGen = do
--   a <- arbitrary
--   return (BoolConj a)

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

-- #7. BoolDisj
newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) =
    BoolDisj False
  (<>) (BoolDisj _) (BoolDisj _) =
    BoolDisj True

boolDisjGen :: Gen BoolDisj
boolDisjGen = BoolDisj <$> arbitrary
-- boolDisjGen = do
--   a <- arbitrary
--   return $ BoolDisj a

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- #8. Or a b
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
    (<>) (Snd b) _ = Snd b
    (<>) _ (Snd b) = Snd b
    (<>) _ (Fst a) = Fst a

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a,
         return $ Snd b]

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Or a b) where
    arbitrary = orGen

type OrAssoc =
       Or Int String
    -> Or Int String
    -> Or Int String
    -> Bool

-- #9. Combine a b
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
     (Combine f) <> (Combine f') = Combine (f <> f')

combineGen :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
combineGen = Combine <$> arbitrary
-- combineGen = do
--   f <- arbitrary
--   return $ Combine f

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = combineGen

instance (CoArbitrary b, Arbitrary a) => CoArbitrary (Combine a b) where
  coarbitrary (Combine f) = variant 0 . coarbitrary f

funEq :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEq (Combine f) (Combine f') = property $ \a -> f a === f' a

instance (Show a, Show b) => Show (Combine a b) where
  show (Combine _) = show "Function Combine a -> b"

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

combineAssoc :: (Arbitrary a, Show a, Eq b, Show b, Semigroup b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEq` (f <> (g <> h))

-- #10. Comp a
newtype Comp a =
  Comp { unComp :: a -> a }

instance Semigroup a
  => Semigroup (Comp a) where
     (Comp f) <> (Comp f') = Comp (f <> f')

compGen :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
compGen = Comp <$> arbitrary
-- compGen = do
--   f <- arbitrary
--   return $ Comp f

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = compGen

instance (CoArbitrary a, Arbitrary a) => CoArbitrary (Comp a) where
  coarbitrary (Comp f) = variant 0 . coarbitrary f

funEq' :: (Arbitrary a, Show a, Eq a, Show a) => Comp a -> Comp a -> Property
funEq' (Comp f) (Comp f') = property $ \a -> f a === f' a

instance Show (Comp a) where
  show (Comp _) = show "Function Comp a -> a"

type CompAssoc a = Comp a -> Comp a -> Comp a -> Property

compAssoc :: (Arbitrary a, Show a, Eq a, Show a, Semigroup a) => CompAssoc a
compAssoc f g h = ((f <> g) <> h) `funEq'` (f <> (g <> h))

main :: IO ()
main = do
  putStrLn "\nTest Trivial Association"
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn "\nTest Identity Association"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  putStrLn "\nTest Two Association"
  quickCheck (semigroupAssoc :: TwoAssoc)
  putStrLn "\nTest Three Association"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn "\nTest Four Association"
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn "\nTest BoolConj Association"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "\nTest BoolDisj Association"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn "\nTest Or Association"
  quickCheck (semigroupAssoc :: OrAssoc)
  putStrLn "\nTest Combine Association"
  quickCheck (combineAssoc :: CombineAssoc (Sum Int) (Sum Int))
  putStrLn "\nTest Comp Association"
  quickCheck (compAssoc :: CompAssoc (Sum Int))
```