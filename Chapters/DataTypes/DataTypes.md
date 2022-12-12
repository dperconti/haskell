# 4 Basic Data Types
---

A *data type* is a set of values with an abstract commonality. A *data declaration* defines a new data type. For example, the data type `Bool` is defined with the following _data declaration_.

```haskell
data Bool = False | True
```

*Pattern matching* is a feature of Haskell that allows multiple implementations of the same function. When calling the function, the implementation will be chosen depending on the argument. `_` is called _catch-all_ and will match any argument value.

*Typeclasses* is a polymorphic type that adds functionality (i.e. faculties or interfaces) to types that is reusable across all inheriting types. A *type alias* is a way of making a type available through a different name: `type Name = Integer`.

The *ordering typeclass* `Ord` enforces implementation of the following operators.

```haskell
compare :: a -> a -> Ordering
(<) :: a -> a -> Bool
(<=) :: a -> a -> Bool
(>) :: a -> a -> Bool
(>=) :: a -> a -> Bool
max :: a -> a -> a
min :: a -> a -> a
```

Haskell’s inequality symbol is `/=`. The *equality typeclass* `Eq` requires the following.

```haskell
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

A *typeclass constraint* can be made for parameters with the following syntax (here for the function equality operator which requires both operands to implement `Eq`): `(==) :: Eq a => a -> a -> Bool`.

Variables in type signatures are commonly named according to the following rules:
1. Type variables are called `a`, `b`, …;
2. function variables are called `f`, `g`, …
3. Arguments to functions are often called `x`, `y`, and `z`.
4. Lists of `x` values are called `xs`.
5. All names can also occur with numbers or the prime symbol appended to them, e.g. `x1` or `f`'.

## 4.1 Numbers
Numbers are inheriting from the typeclass `Num`.

- `Int`. An integral number (aka. integer) with a fixed precision, that is it has upper and lower bound (size: 8 byte). `GHC.Int` adds the integer types `Int8`, `Int16`, `Int32`, and `Int64`, with the number indicating the number of bits used to store the value. The value range of Int is `[-9223372036854775808, 9223372036854775807]`.
- `Integer`. An integral number that supports arbitrarily large or small numbers.
- `Float`. Single-precision floating point number (size: 4 byte).
- `Double`. Double-precision floating point number (size: 8 byte).
- `Rational`. Represents a fraction of two integer numbers. The data type wraps two Integers and is hence arbitrarily precise.
- `Scientific`. Floating point number with an `Integer` base and `Int` exponent. Therefore, the numbers can be arbitrarily large and precise. This data type is not part of GHC and must be installed separately (`stack install scientific`).

The `Integer` type should be preferred over `Int`, and `Scientific` and `Rational` (typeclass `Fractional`) should be preferred over `Float` and `Double`, unless computational efficiency matters.

## 4.2 Boolean
The boolean data type can either be `True` or `False` and is defined as `data Bool = False | True`. Operators for booleans are `&&` for *and*, `||` for *or*, and the function `not` for *inversion*.

Haskell features *if expressions* with the following syntax: `if <condition> then <a> else <b>`. The entire if expression evaluates to either `<a>` or `<b>`, depending on the condition.

## 4.3 Tuples
*Tuples* are types that store a fixed number n of constituents which may have different types themselves. n is referred to as arity (number of parameters that a function takes). A tuple can be created with its constructor, `(,,) x1 x2 x3`, here with n=3. Tuples with n=1 must not exist, however, n=0 is possible and called unit `()`.

For convenience, the first element in a tuple can be accessed using `fst :: (a, b) -> a`; `snd` serves equally for the second value. `Data.Tuple` contains the tuple manipulation functions `curry`, `uncurry`, and `swap`.

A tuple can be unpacked when passed to a function with the following syntax: `tupleSum (a, b) = a + b`

## 4.4 Lists
The *list* data type stores _n_ values of equal type, where n can be changed dynamically.

The n-th element of a list can be accessed with the `!!` operator (`n` is zero based): `"abc" !! n`.