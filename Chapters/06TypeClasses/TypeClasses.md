# 6 Typeclasses
*Typeclasses* generalize over a set of types in terms of consumption or usage in computation. After declaring a data type with the `data Typename` keyword, typeclasses can be assigned with e.g. `instance Typeclass Typename`. Typeclasses can *inherit* from a _superclass_ (e.g.` class Num a => Fractional a`).

A typeclass can be defined with the `class` keyword. The `Num` typeclass for example is defined as follows.

```haskell
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

Types which implement the `Integral` type are also required to implement `Real` and `Enum`.

```haskell
class (Real a, Enum a) => Integral a
```

A typeclass is implemented for a type with `instance`. The implementation is called *instance* and might look like this.

```haskell
data Suit = Spade | Diamond | Club | Heart
instance Eq Suit where
  (==) Spade Spade = True
  (==) Diamond Diamond = True
  (==) Club Club = True
  (==) Heart Heart = True
  (==) _ _ = False
```

Typeclasses *default* to certain types. `Num` defaults to `Integer` for example default Num Integer. This can be better explained given an example: When entering a `5` into GHCi, a show method must be called. `:t 5` however gives `Num` so it is left to Haskell to choose a `show` method from the inheriting types. In this case `Integer` is chosen by default.

*Typeclass instances* are unique pairings of a typeclass and a type.

*Effects* are observable actions programs may take, such as writing to a file or printing to the console. `IO` is the type for values whose evaluation bears the possibility of causing side effects.

## 6.1 Derivable Typeclasses
The following typeclasses can be automatically derived. That means they can be automatically instantiated for a given type, based on how it is defined.

- `Bounded`. Types that have an upper and lower bound.
- `Enum`. The type’s values can be enumerated. Provides methods such as succ (successor; comparable to incrementing), pred (predecessor), enumFromTo, and enumFromThenTo (which uses a step size based on the second argument).
- `Eq`. The type’s values can be tested for equality.
- `Ord`. The type’s values can be put into sequential order. Implies Eq and can be implemented by defining the compare method which returns EQ, LT, or GT.
- `Read`. Values can be parsed from strings. It is often a partial function as it does not return a proper value for all possible inputs.
- `Show`. Values can be converted to strings (e.g. for output). Enforces implementation of `showsPrec`, `show`, and `showList`. Printing things is possible in Haskell, even though it is purely functional, because the `print` method invokes `IO` which has the side effect of outputting text. It returns the unit `()` because it has no relevant return value.

## 6.2 Typeclass Inheritance
Inheritance structure of common typeclasses. `Ord` inherits from `Eq`. `Real` inherits from `Ord` and `Num`. `Fractional` inherits from `Num`. `Integral` inherits from `Real`, `Fractional`, and `Enum`.


-   Typeclasses and types in Haskell are, in a sense, opposites: where a declaration of a type defines how that type in particular is created, a declaration of a typeclass defines how a set of types are consumed or used in computations.

-   When you have a typeclass-constrained (ad-hoc) polymorphic value and need to evaluate it, the polymorphism must be resolved to a specific concrete type. But in some cases, particularly when you’re working in the GHCi REPL you will not have specified a concrete type for a polymorphic value. In those situations, the typeclass will default to a concrete type, and the default types are already set in the libraries.

-   The Haskell Report3 specifies the following defaults relevant to numerical computations:
    -   default Num Integer
    -   default Real Integer
    -   default Enum Integer
    -   default Integral Integer
    -   default Fractional Double
    -   default RealFrac Double
    -   default Floating Double
    -   default RealFloat Double
-   The use of polymorphic values without the ability to infer a specific type and no default rule will cause GHC to complain about an ambiguous type.

-   Haskell introduced and refined a means of writing ordinary programs that talk to the outside world without adding anything to the pure lambda calculus it is founded on. This property -- being lambda calculus and nothing more -- is what makes Haskell a purely functional programming language.

-   Typeclass instances we can magically derive are `Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`, though there are some constraints on deriving some of these.

-   Read is a partial function, a function that doesn't return a proper value as a result for all possible inputs.

-   If we turn all warnings on with the `Wall` flag in our REPL or in our build configuration, then GHC will let us know when we’re not handling all cases: `:set -Wall`