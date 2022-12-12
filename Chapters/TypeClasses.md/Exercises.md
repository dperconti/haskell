Exercises
---------

*Multiple choice*
1.  c
2.  a, c
3.  a
4.  c
5.  a

*Does it typecheck?*

``` haskell
data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

<interactive>:66:1: error:
    • No instance for (Show (() -> IO ()))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
```

```haskell
-- 2. No. No instance of Eq.
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

<interactive>:83:21: error:
    • No instance for (Eq Mood) arising from a use of ‘==’
```

```haskell
If you were able to get settleDown to type check:
a) What values are acceptable inputs to that function?
b) What will happen if you try to run settleDown 9? Why?
c) What will happen if you try to run Blah > Woot? Why?

Blah, Woot only.
- Error. Number cannot be tested equal with Mood.
- Error. No instance of Ord.
```

```haskell
-- 4. Yes.

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
              deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
```

**Given a datatype declaration, what can we do?**

``` haskell
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1. No
phew = Papu (Rocks "chases") (Yeah True)

-- 2. Yes
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. Yes
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. No. No instance of Ord.
```

**Match the types**

1.  No - should be instance of Num
2.  No - should be instance of Fractional
3.  Yes.
4.  Yes.
5.  Yes.
6.  Yes.
7.  No - should be Int.
8.  No - be Int.
9.  Yes.
10. Yes.
11. No - type cannot be changed.

**Type-Kwon-Do Two: Electric Typealoo**

``` haskell
--1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

--2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = fromInteger i + (f a)
```