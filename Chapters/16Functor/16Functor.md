## 16 Functor
A **functor** is a structure preserving mapping. Such a mapping requires a function that is applied to each of the values that the wrapping type encloses. A functor satisfies that for an identity mapping, the values remain the same, also the composition law `fmap (f . g) == fmap f . fmap g` holds. The infix operator for `fmap` is `<$>`.

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

Applying a function to a value that is inside of a structure is refered to as **lifting**.

For nested **functor application**, e.g. when applying a function to characters which are stored in a list of `Strings`, a syntax such as `(fmap . fmap) strFn dataStruct` can be used.

In order to use a higher kinded Type, e.g. `* -> * -> *`, as a `Functor`, one of the type parameters has to be applied. This can either be done with a concrete type such as `Integer` or with a type variable `a`, and results in the kind `* -> *`. Sample snippet:

```haskell
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
```

A **natural transformation** is changing the structure while preserving the content.

```haskell
{-# LANGUAGE RankNTypes #-}
type Nat f g = forall a . f a -> g a
```
A **functor** is a way to apply a function over or around some structure that we don’t want to alter.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

### functor laws
- Identity: `fmap id == id`
- Composition: `fmap (f . g) == fmap f . fmap g`

Unlike **Monoid**, Functor instances will be unique for a datatype, in part because of **parametricity**, in part because arguments to type constructors are applied in order of definition. In a hypothetical not-Haskell language, other cases might be possible.

In functional programming, a functor is a design pattern inspired by the definition from category theory that allows one to apply a function inside a generic type without changing the structure of the generic type. This idea is encoded in Haskell using the type class

This was a super helpful read:
[Medium Article](https://medium.com/openmindonline/js-monday-09-understanding-functors-43a426d34e26)

Functor in Haskell is a kind of functional representation of different Types which can be mapped over. It is a high level concept of implementing polymorphism. According to Haskell developers, all the Types such as List, Map, Tree, etc. are the instance of the Haskell Functor.

A Functor is an inbuilt class with a function definition like −

```haskell
class Functor f where
   fmap :: (a -> b) -> f a -> f b
```