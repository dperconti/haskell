# 11 Algebraic Data Types

**Type constructors** are used at the type level, in type signatures, typeclass declarations, and instances. They are static and resolved at compile time. **Data constructors** construct values and can be interacted with at runtime. Type and data constructors with no arguments are **constants**, for instance `Bool`.

The **arity** of a constructor is the number of parameters it takes. A type or data constructor with no arguments are called _nullary_ and are _type constant_. Data constructors that take exactly one argument are called _unary_, with more than one they are referred to as products.

A type constructor argument that does not occur alongside with any value constructor is called **phantom**. For example `a` is a phantom in the declaration data Type `a = Value`.

The **record syntax** allows for the definition of types, where the contained values have names. For example `data Person = Person { name :: String, age :: Int }`. The values can then be accessed by e.g. `name person`.

We can declare custom types for data in Haskell using the `data` keyword. Consider the following declaration of our familiar cons list:

```haskell
data ConsList = Nil | Cons Int ConsList
```

The `|` operator looks rather like the union type operator in TypeScript, and indeed it serves a similar purpose. Here, a `ConsList` is defined as being a composite type, composed of either `Nil` or a `Cons` of an `Int` value and another `ConsList`. This is called an “algebraic data type” because `|` is like an “or”, or algebraic “sum” operation for combining elements of the type while separating them with a space is akin to “and” or a “product” operation.

Note that neither `Nil` or `Cons` are built in. They are simply labels for constructor functions for the different versions of a `ConsList` node. You could equally well call them `EndOfList` and `MakeList` or anything else that’s meaningful to you. `Nil` is a function with no parameters, `Cons` is a function with two parameters. `Int` is a built-in primitive type for limited-precision integers.

Now we can create a small list like so:

```haskell
l = Cons 1 $ Cons 2 $ Cons 3 Nil
```


Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two basic operations: sum and product.

cardinality
- Nullary constructors represent one value
- Unary constructor always have the same cardinality as the type they contain.
- Sum types are + or addition.
- Product types are product or multiplication.
- Function type is the exponent operator.

`newtype`: define a type that can only ever have a single unary data constructor.

- A advantages over a vanilla data declaration: no runtime overhead, as it reuses the representation of the type it contains. The difference between newtype and the type it contains is gone by the time the compiler generates the code.
- One key contrast between a newtype and a type alias is that you can define typeclass instances for newtypes that differ from the instances for their underlying type.

Language pragmas, also called extensions, are special instructions to the compiler. They tell the compiler to process input in ways beyond what the standard provides for.

A language pragma `GeneralizedNewtypeDeriving` allow our newtype to rely on a typeclass instance for the type it contains.

Whenever we have a product that uses record accessors, keep it separate of any sum type that is wrapping it. To do this, split out the product into an independent type with its own type constructor.

