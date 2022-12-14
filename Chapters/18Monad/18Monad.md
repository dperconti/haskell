# 18 Monad

Monad is a typeclass reifying an abstraction that is commonly used in Haskell. Instead of an ordinary function of type `a` to `b`, it is functorially **applying a function which produces more structure itself** and **using join to reduce the nested structure that results**. In other words, it is the process of taking a function that converts a value of type `a` into another type `(b)`, wrapped within a third type `c`. This function is applied to a value (of type `a`) wrapped within `c`. The resulting structure is then reduced from `c c b` to `c b`.

```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
```

`>>=` is called _bind_ operator. Intuitively it can be understood as given a couple of wrapped values and a function that can be applied to these, the bind operator applies the function to each of the values. Special about it is (compared to `fmap`) that the argument order is flipped and the mapping function returns a monad itself which is joined to make sure the output is not nested. The application to the list monad clarifies what that means: `(>>=) :: [a] -> (a -> [b]) -> [b]`.

`*>` for `Applicative` corresponds to `>>` for `Monad`. The `do` syntax is converted into each line being _concatenated_ with the following line using one of the two operators. Variable assignments `<-` are converted to `>>=`, for example

```haskell
do
  name <- getLine
  putStrLn name
```

becomes the following:

```haskell
getLine >>= \name -> putStrLn name
```

`Control.Monad` contains a `join` function. The book introduced it with the example `join $ putStrLn <$> getLine`, which would, without `join`, fail because of nested `IO`s.

Example of using the `do` syntax in combination with the `List` monad:

```haskell
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]
```

The Monad **laws** are

- Right identity `m >>= return = m`. Applying return leaves the data untouched.
- Left identity `return x >>= f = f x`. Applying `return` leaves the data untouched.
- Associativity `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`. Regrouping the functions should not have any impact on the final result.

- Left Identity Law ??? The return function does not change the value and it should not change anything in the Monad. It can be expressed as "return >=> mf = mf".
- Right Identity Law ??? The return function does not change the value and it should not change anything in the Monad. It can be expressed as "mf >=> return = mf".
- Associativity ??? According to this law, both Functors and Monad instance should work in the same manner. It can be mathematically expressed as "( f >==>g) >=> h =f >= >(g >=h)".

Using Checkers (as in 17.2) with `quickBatch (monad [(a, b, c)])` where `a`, `b`, and `c` are three values which indicate the type to be used.

The **Kleisli composition** (_fish_ operator: >=>) is about composing two functions which both return monads. It can be imported with import `Control.Monad ((>=>))` and has the following signature (in comparison to normal function composition):

```haskell
(.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

Given a `Monad` instance, the instances for `Functor` and `Applicative` can be implemented automatically, as shown in the following snippet.

```haskell
instance Functor (State s) where
  fmap = Control.Monad.liftM

instance Applicative (State s) where
  pure = return
  (<*>) = Control.Monad.ap
```
### Reiteration Attempt

-   A Monad is an applicative functor. Monad is "stronger" than Applicative, and Applicative is stronger than Functor.

``` haskell
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- define Functor in items of Monad
fmap_m :: (Monad m) => (a -> b) -> (m a) -> (m b)
fmap_m f xs = xs >>= return . f

-- define Applicative in terms of Monad
pure_m :: (Monad m) => a -> m a
pure_m = return

ap_m :: (Monad m) => m (a -> b) -> m a -> m b
ap_m f xs = xs >>= (\x -> f >>= (\g -> return (g x)))
```

-   The unique part of Monad is the following function:

``` haskell
-- join :: Monad m => m (m a) -> m a
```

-   Define bird operation in terms of fmap and join:

``` haskell
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join (fmap f x)
```

-   With the Maybe Applicative, each Maybe computation fails or succeeds independently of each other; With the Maybe Monad, computations contributing to the final result can choose to return Nothing based on "previous" computations.

-   Laws:
    -   identity: return should be neutral and not perform any computation.
        -   right identity: `m >>= return = m`
        -   left identity: `return x >>= f = f x`
    -   associativity: `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`
-   Kleisli composition.

``` haskell
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```