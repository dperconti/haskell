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


## Exercises: Vehicles

```haskell
main :: IO ()

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir
```

1. What is the type of `myCar`?
```haskell
❯ runhaskell TypeTest.hs
type is: Vehicle
```

2. Given the following, define the functions:

```haskell
isCar :: Vehicle -> Bool
-- Based on the definition of `Vehicle`, this will either be a Car or a Place. We pattern match off of Car, and return True
-- Throw away the rest of the values, and have a default of False
isCar (Car _ _) = True
isCar _ = False
-- Verified by `isCar myCar`

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False
-- Verified by `isPlane doge`

areCars :: [Vehicle] -> [Bool]
-- We can just use map to map the function `isCar` to the list of Vehicles
areCars = map isCar
```

3.

```haskell
getManu :: Vehicle -> Manufacturer
-- in order to get the Manufacturer, which only exists on Vehicle of type Car, we must pattern match on both of those values
getManu (Car m _) = m
-- getMany _ = "Not a Car" -- I would need to use Either here, but let's not get into that now
```

4. No exhaustive cases error

5.

```haskell
data Vehicle = Car Manufacturer Price | Plane Airline Integer
```

## Exercises: Cardinality

1.
```haskell
data PugType = PugData
-- Answer is 1
```

2. For this one, recall that `Bool` is also defined with the `|` symbol:

```haskell
data Airline =
      PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
-- Answer is 3
```

3. Given what we know about `Int8`, what’s the cardinality of `Int16`? The cardinality of `Int16` is `2^16 = 65536`.
4. Use the REPL and `maxBound` and `minBound` to examine Int and Integer. What can you say about the cardinality of those types?
  1. The cardinality of `Int` on my computer is `2^64 = 18446744073709551616`. The cardinality of `Integer` is infinite since `Integer` is not bound.
5. Extra credit (impress your friends!): what’s the connection between the `8` in `Int8` and that type’s cardinality of `256`? `2^8`

