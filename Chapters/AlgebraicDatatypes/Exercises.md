# Exercises

## Exercises: Dog types

1. Is Doggies a type constructor or a data constructor?
Type Constructor

2. What is the kind of Doggies?
```haskell
Prelude> :k Doggies
Doggies :: * -> *
```

3. What is the kind of Doggies String?
```haskell
*
```

4. What is the type of Husky 10?
```haskell
Num a => Doggies a
```

5. What is the type of Husky (10 :: Integer)? 6. What is the type of Mastiff "Scooby Doo"?

```haskell
-- Wasn't sure about this one. This can be verified with this, though:
-- putStrLn ("type is: " ++ (show (typeOf (Mastiff "Scooby Doo"))))
❯ runhaskell TypeTest.hs
type is: Doggies [Char]
```

7. Is DogueDeBordeaux a type constructor or a data constructor?

> So, earlier on in the chapter is this: "DogueDeBordeaux is a type constructor and has a single type vari- able argument like HuskyType, but called doge instead of a. Why? Because the names of variables don’t matter. At any rate, this type also enumerates one constructor."
> But, I also found this online as well:
> It is a type constructor if appear in type level, and is a data constructor if appear in term level.
> So... answer is both?

8. What is the type of DogueDeBordeaux?

```haskell
a -> DogueDeBordeaux a
```

9. What is the type of DogueDeBordeaux "doggie!"

```haskell
❯ runhaskell TypeTest.hs
type is: DogueDeBordeaux [Char]
```