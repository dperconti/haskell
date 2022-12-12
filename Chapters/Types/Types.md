# 5 Types
---

Type systems have been defined to enforce correctness. In Haskell, typing is _static_ and typechecking occurs at _compile time). A *data type declaration* defines a _type constructor_ and _data constructors_. Haskell functions are created from the function type constructor `->` and the function is a value.

A function signature may have multiple *typeclass constraints* `(Num a, Num b) => a -> b -> b`. In the example, `a` could be an `Integer` and both `bs` could be `Double`s. However, different types for the second argument and the return type would not be possible with this definition.

The `=>` is called *typeclass arrow*. The right associative *type constructor for functions* `->` realizes currying: `f :: a -> a -> a` is read as `f :: a -> (a -> a)`. Due to currying, functions can be partially applied. Infix operators can be partially applied to a first or second parameter, e.g. `(2^)` or `(^2)`.

*Polymorphism* is the provision of a single interface to entities of different types. In Haskell it is either _parametric_ or _constrained_ (aka. _bounded_, _ad-hoc_). The former is polymorphism that accepts any type, whereas the latter accepts only some types. Multiple class constrains must be wrapped in parentheses: `f :: (Eq a, Num b) => a -> b`. The opposite of polymorphism is monomorphism, in Haskell called concrete. Applied to variables, polymorphism is a property of variables which may refer to more than one concrete type.

*Type inference* is the process of determining a variables principle type by looking at the way it is being used. The *principle type* is the most generic type that can be assigned to a variable.

Type signatures may have three kinds of types:

- concrete
- constrained polymorphic: also called ad-hoc polymorphism. Implemented with typeclasses; decreasing the number of concrete types it could be, but increasing what you can actually do with it by defining and bringing into scope a set of operations.
- parametrically polymorphic: refer to type variables, or parameters. Parametricity means that the behavior of a function with respect to the types of its (parametrically polymorphic) arguments is uniform. The behavior can not change just because it was applied to an argument of a different type.