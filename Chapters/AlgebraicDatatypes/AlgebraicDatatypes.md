# 11 Algebraic Data Types

**Type constructors** are used at the type level, in type signatures, typeclass declarations, and instances. They are static and resolved at compile time. **Data constructors** construct values and can be interacted with at runtime. Type and data constructors with no arguments are **constants**, for instance `Bool`.

The **arity** of a constructor is the number of parameters it takes. A type or data constructor with no arguments are called _nullary_ and are _type constant_. Data constructors that take exactly one argument are called _unary_, with more than one they are referred to as products.

A type constructor argument that does not occur alongside with any value constructor is called **phantom**. For example `a` is a phantom in the declaration data Type `a = Value`.

The **record syntax** allows for the definition of types, where the contained values have names. For example `data Person = Person { name :: String, age :: Int }`. The values can then be accessed by e.g. `name person`.