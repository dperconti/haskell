# Exercises

## Short Exercise: Either Monad

Implement the Either Monad:

```haskell
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = undefined

instance Applicative (Sum a) where
  pure = undefined
  (<*>) = undefined

instance Monad (Sum a) where
  return = pure
  (>>=) = undefined
```

```haskell
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure a = Second a
  (<*>) (First n) _ = First n
  (<*>) _ (First n) = First n
  (<*>) (Second f) (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (>>=) (First n) _ = First n
  (>>=) (Second n) f = f n
```

## Chapter Exercises

Write Monad instances for the following types. Use the QuickCheck properties we  showed you to validate your instances.

1. Welcome to the Nope Monad, where nothing happens and nobody cares:

```haskell
data Nope a = NopeDotJpg

-- We're serious. Write it anyway.
```

```haskell
data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg
```

2.

```haskell
data BahEither b a =
  PLeft a
  | PRight b
```

```haskell
data PhhhbbtttEither b a =
    Left' a
  | Right' b

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b
  (<*>) (Left' f) (Left' x) = Left' (f x)

instance Monad (PhhhbbtttEither b) where
  (>>=) (Right' b) _ = Right' b
  (>>=) (Left' x) f = f x
```

3. Write a Monad instance for Identity:

```haskell
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure = undefined
  (<*>) = undefined

instance Monad Identity where
  return = pure
  (>>=) = undefined
```

```haskell
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x
```

4.  This one should be easier than the Applicative instance was. Remember to use the Functor that Monad requires, then see where the chips fall:

```haskell
data List a =
  Nil
  | Cons a (List a)
```

```haskell
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) fs xs = flatMap (\f -> fmap f xs) fs

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = append (f x) (xs >>= f)
```

Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and composition is fine, but it has to type check with the types provided:

1.

```haskell
j :: Monad m => m (m a) -> m a
```

Expecting the following behavior:

```shell
Prelude> j [[1, 2], [], [3]]
[1,2,3]
Prelude> j (Just (Just 1))
Just 1
Prelude> j (Just Nothing)
Nothing
Prelude> j Nothing
Nothing
```

```haskell
j :: Monad m => m (m a) -> m a
j x = x >>= id
```

2.

```haskell
l1 :: Monad m => (a -> b) -> m a -> m b
```

```haskell
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= (return . f)
```

3.
```haskell
l2 :: Monad m
  => (a -> b -> c) -> m a -> m b -> m c
```

```haskell
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = a >>= (\a1 -> b >>= (\b1 -> return (f a1 b1)))
```

4.
```haskell
a :: Monad m => m a -> m (a -> b) -> m b
```

```haskell
a :: Monad m => m a -> m (a -> b) -> m b
a x f = x >>= (\x1 -> f >>= (\f1 -> return (f1 x1)))
```

5. You’ll need recursion for this one:
```haskell
meh :: Monad m
  => [a] -> (a -> m b) -> m [b]
```


```haskell
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = l2 (:) (f x) (meh xs f)
```

6. Hint: reuse meh:

```haskell
flipType :: (Monad m) => [m a] -> m [a]
```

```haskell
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs i
```

