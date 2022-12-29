# 15 Monoid and Semigroup

## 15.1 Monoid

A **monoid** is a binary associative operation with an identity. In other words, it is an operator that takes two arguments that follow the rules associativity and identity.

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty, mappend #-}
```

Monoids are all types that let you join values together through the `mappend` function, in accordance with associativity. A `mempty` value exists for which the `mappend` becomes the identity.

Much more extended functionality lies in the **package** `Data.Monoid`. Opposed to many other Haskell typeclasses, monoids do often have multiple implementations per type. That is realized by wrapping the type with `newtype`. For example the `newtype` Sum, which wraps `Num`s and determines to use the addition monoid for the wrapped value. Calling `mappend` with two `Product` values, however, would multiply them. The resulting type wraps the sum or the product. The actual number can be retrieved through `getSum` and `getProduct` respectively. Similarly, the `Bool` monoid is wrapped in either `Any` (boolean disjuction) or `All` (boolean conjunction).

`mconcat` applies `mappend` to an arbitrary number of values. For the empty list it returns `mempty`, for a list with one entry it is the identity.

The **Abelian monoid** has the commutative property, i.e. for all _x_, _y_, `mappend x y == mappend y x` holds.

An **orphan instance** is an instance that is defined for a datatype and a typeclass, but not in the same module as either of them. If neither typeclass nor datatype were defined manually, the best workaround is to create a `newtype` which wraps the datatype.

## 15.2 Semigroup

A **semigroup** (Haskell package `Data.Semigroup`) is a monoid without the identity property. That is an operation which takes two inputs and reduces them to one, and suffices the law of associativity. In code, that means the semigroup defines

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

while satifying associativity, i.e. `(a <> b) <> c == a <> (b <> c)`.

The `NonEmpty` datatype resides in `Data.List.NonEmpty`. It is a list that contains one or more elements.