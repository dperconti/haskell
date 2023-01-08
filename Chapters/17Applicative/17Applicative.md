# 17 Applicative
An **applicative** is a monoidal functor. Opposed to `fmap`, with `<*>` the function (that is applied to the enclosed values) is inside a functor itself. Intuitively this can be understood as _mapping a plurality of functions over a plurality of values_. The type info is the following:

```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

The function `pure` can be though of as _embedding a value into any structure (functor)_. For example `pure 1 :: [Int] gives [1]`.

An **Applicative** satisfies the following four laws:

1. Identity: `pure id <*> v = v`
2. `Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
3. Homomorphism (structure preserving): `pure f <*> pure x = pure (f x)`
4. Interchangeability: `u <*> pure y = pure ($ y) <*> u`

## 17.1 Examples

| Command | Result |
| -- |-- |
| `(,) <$> [1, 2] <*> [3, 4]` | `[(1,3),(1,4),(2,3),(2,4)]` |
| `(+) <$> [1, 2] <*> [3, 5]` | `[4,6,5,7]` |
| `liftA2 (+) [1, 2] [3, 5]` | `[4,6,5,7]` |

## 17.2 Testing

Validating whether a data structure satisfies the mentioned laws can be done with the [checkers](https://github.com/haskell-checkers/checkers) package. The following snippets validates an **Applicative**. Note that the value is not actually being used. Its purpose is to indicate which types to validate.

```haskell
module ApplicativeTests where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

list = [("b", "w", 1)]

main = do
  quickBatch $ applicative list
```

## 17.3 Maybe
Haskell Prelude implementation of `Maybe`‘s **Applicative** instance ([source](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Base.html)).

```haskell
-- | @since 2.01
instance Applicative Maybe where
  pure = Just

  Just f  <*> m       = fmap f m
  Nothing <*> _m      = Nothing

  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ _ _ = Nothing

  Just _m1 *> m2      = m2
  Nothing  *> _m2     = Nothing
```


## Applicative Functor

An Applicative Functor is a normal Functor with some extra features provided by the Applicative Type Class.

Using Functor, we usually map an existing function with another function defined inside it. But there is no any way to map a function which is defined inside a Functor with another Functor. That is why we have another facility called **Applicative Functor**. This facility of mapping is implemented by Applicative Type class defined under the **Control** module. This class gives us only two methods to work with: one is **pure** and the other one is **<*>**.

Following is the class definition of the Applicative Functor.

```haskell
class (Functor f) => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b
```

According to the implementation, we can map another Functor using two methods: "Pure" and "<*>". The "Pure" method should take a value of any type and it will always return an Applicative Functor of that value.

The following example shows how an Applicative Functor works

```haskell
import Control.Applicative

f1:: Int -> Int -> Int
f1 x y = 2*x+y
main = do
   print(show $ f1 <$> (Just 1) <*> (Just 2) )
```

Here, we have implemented applicative functors in the function call of the function f1. Our program will yield the following output.
```haskell
"Just 4"
```


Applicative is where the function we are applying is also embedded in some structure.

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Type has Applicative must have Functor.

```haskell
fmap f x = pure f <*> x
```

Applicative functors are monoidal functors. the `f` is the monodial part, and the map behavior is the functorial part.
