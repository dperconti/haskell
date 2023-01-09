# Exercises

## Exercises: Lookups

Make the following expressions type check:
1.
```haskell
added :: Maybe Integer added =
(+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
```
```haskell
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
```

2.
```haskell
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) y z
```
```haskell
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

tupled' :: Maybe (Integer, Integer)
tupled' = pure (,) <*> y <*> z
```

3.

```haskell
import Data.List (elemIndex)

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int max' = max
maxed :: Maybe Int maxed = max' x y
```

```haskell
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'
```

4.

```haskell
xs = [1, 2, 3] ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer summed = sum $ (,) x y
```
```haskell
xs = [1, 2, 3]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x2 <*> y2
```

## Exercise: Identity Instance

Write an Applicative instance for Identity:

```haskell
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure = undefined
  (<*>) = undefined
```

## Exercise: Constant Instance

Write an Applicative instance for Constant:

```haskell
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap = undefined

instance Monoid a => Applicative (Constant a) where
  pure = undefined
  (<*>) = undefined
```

```haskell
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant (mempty)
  (<*>) (Constant e1) (Constant e2) = Constant (mappend e1 e2)
```

## Exercise: Fixer upper
Given the functions and values provided, use <$> from Functor and <*> and pure from the Applicative type class to fill in missing bits of the broken code below to make it work:

1.
```haskell
const <$> Just "Hello" <*> "World"
```

```haskell
e1 = const <$> Just "Hello" <*> pure "World"
```

2.

```haskell
(,,,) Just 90
<*> Just 10 Just "Tierness" [1, 2, 3]
```

```haskell
e2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
```

## List Applicative exercise

Implement Applicative for lists. Writing a minimally complete Applicative instance calls for writing the definitions of both pure and <*>. We’re going to provide a hint, as well. Use the checkers library to validate your Applicative instance:

```haskell
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)
```

Remember what you wrote for the list Functor:

```haskell
instance Functor List where
  fmap = undefined
```

Writing the list Applicative is similar:

```haskell
instance Applicative List where
  pure = undefined
  (<*>) = undefined
```

```haskell
module ListApplicative where

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

instance Semigroup (List a) where
    xs <> ys = append xs ys

instance Semigroup a => Monoid (List a) where
    mempty = Nil
    mappend = append

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)
```

## Chapter Exercises

Given a type that has an instance of Applicative, specialize the types of the methods. Test your specialization in the REPL. One way to do this is to bind aliases of the type class methods to more concrete types that have the type we tell you to fill in:

1.

```haskell
-- Type
[]
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

```haskell
pure :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]
```

2.

```haskell
-- Type IO
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

```haskell
pure ::a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b
```

3.

```haskell
-- Type (,) a
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

```haskell
pure :: Monoid b => a -> (b, a)
(<*>) :: Monoid c => (c, (a -> b)) -> (c, a) -> (c, b)
```

4.

```haskell
-- Type (->) e
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

```haskell
pure :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
```


Write instances for the following datatypes. Confused? Write out what the types should be. Use the checkers library to validate the instances:

1.
```haskell
data Pair a = Pair a a deriving Show
```

```haskell
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)
```

2.

This should look familiar:

```haskell
data Two a b = Two a b
```

```haskell
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a1 f) (Two a2 x) = Two (mappend a1 a2) (f x)

```

3.
```haskell
data Three a b c = Three a b c
```

```haskell
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a1 b1 f) (Three a2 b2 x) = Three (mappend a1 a2) (mappend b1 b2) (f x)
```

4.
```haskell
data Three' a b = Three' a b b
```

```haskell
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' b1 f1 f2) (Three' b2 x1 x2) = Three' (mappend b1 b2) (f1 x1) (f2 x2)
```

5.

```haskell
data Four a b c d = Four a b c d
```

```haskell
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a1 b1 c1 f) (Four a2 b2 c2 x) =  Four (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (f x)
```

6.
```haskell
data Four' a b = Four' a a a b
```

```haskell
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a1 a2 a3 f) (Four' b1 b2 b3 x) = Four' (mappend a1 b1) (mappend a2 b2) (mappend a3 b3) (f x)
```

## Combinations

Remember the vowels and stops exercise from Chapter 10, on folds? Write a function to generate all the possible combinations of three input lists, using liftA3 from Control.Applicative:

```haskell

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
```

```haskell
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
```