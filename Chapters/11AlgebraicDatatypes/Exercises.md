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

## Exercises: For Example:
1. What is the type of the data constructor MakeExample? What happens if you request the type of Example?
```haskell
MakeExample :: Example
```
`Example`; it can only be `MakeExample`. No data constructor called `Example` in scope.

2. What if you try :info on Example in GHCi? Can you determine what typeclass instances are defined for the Example type using :info in GHCi?

It returns the definition and also the `typeclass` instances. You also get the list of typeclass instances, and their locations.

3. Try making a new datatype like Example but with a single argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi?

```haskell
MakeExample :: Int -> Example
```

## Exercises: Logic Goats

```haskell
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
```

1. Reusing the `TooMany` typeclass, write an instance of the typeclass for the type `(Int, String)`. This will require adding a language pragma named `FlexibleInstances` if you do not use a newtype — GHC will tell you what to do.

```haskell
instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n
```

2. Make another `TooMany` instance for `(Int, Int)`. Sum the values together under the assumption this is a count of goats from two fields.

```haskell
instance TooMany (Int, Int) where
  tooMany (n, m) = tooMany (n + m)
```

3. Make another `TooMany` instance, this time for `(Num a, TooMany a) => (a, a)`. This can mean whatever you want, such as summing the two numbers together.

```haskell
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = (tooMany n) && (tooMany m)
```

## Exercises: Pity the Bool

1. Given a datatype

```haskell
data BigSmall =
  Big Bool
  | Small Bool deriving (Eq, Show)
```

What is the cardinality of this datatype? Hint: We already know Bool’s cardinality. Show your work as demonstrated earlier.

```haskell
C(BigSmall) = C(Big) + C(Small)

C(BigSmall) = C(Bool) + C(Bool)

C(BigSmall) = 2 + 2

C(BigSmall) = 4
```

2. Given a datatype

```haskell
-- bring Int8 in scope
import Data.Int

data NumberOrBool =
    Numba Int8
    | BoolyBool Bool deriving (Eq, Show)

-- This took way too long to get...
myNumba = Numba (-128)
```

What is the cardinality of NumberOrBool?


```haskell
C(NumberOrBool) = C(Numba) + C(BoolyBool)

C(NumberOrBool) = C(Int8) + C(Bool)

C(NumberOrBool) = 256 + 2

C(NumberOrBool) = 258
```

What happens if you try to create a Numba with a numeric literal larger than 127? And with a numeric literal smaller than (-128)?

> It overflows/undeflows. i.e,

```haskell
Numba 200 == Numba (-56)
Numba (-200) == Numba 56

GHC shows a warning with `-Woverflowed-literals`
```

## Exercises: How Does Your Garden Grow?

1. Given the type

```haskell
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show
```

What is the sum of products normal form of Garden?

```haskell
data Garden' = Gardenia' Gardener
            | Daisy' Gardener
            | Rose' Gardener
            | Lilac' Gardener
            deriving Show
```

## Exercises: Programmers

```haskell
data OperatingSystem
    = GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang
    = Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer = Programmer
    { os :: OperatingSystem
    , lang :: ProgLang
    } deriving (Eq, Show)
```

1. Write a function that generates all possible values of Programmer. Use the provided lists of inhabitants of OperatingSystem and ProgLang.

```haskell
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
   [ GnuPlusLinux
   , OpenBSDPlusNevermindJustBSDStill
   , Mac
   , Windows
   ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Had to look this one up: https://stackoverflow.com/a/32093988
-- Just a list comprehension the same that would be done in a python implementation: https://stackoverflow.com/a/39064769
allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]
```

## Exercises: The Quad

Determine how many unique inhabitants each type has. Suggestion: do the arithmetic unless you want to verify. Writing them out gets tedious quickly.

```haskell
data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)
```

1.
```haskell
eQuad :: Either Quad Quad
eQuad = undefined

C(eQuad) = C(Quad) + C(Quad) = 4 + 4 = 8
```

2.
```haskell
prodQuad :: (Quad, Quad)
prodQuad = undefined

C(prodQuad) = C(Quad) * C(Quad) = 4 * 4 = 16
```

3.

```haskell
funcQuad :: Quad -Quad
funcQuad = undefined

C(funcQuad) = C(Quad) ^ C(Quad) = 4 ^ 4 = 256
```

4.

```haskell
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

C(prodTBool) = C(Bool) * C(Bool) * C(Bool) = 2 * 2 * 2 = 8
```

5.

```haskell
gTwo :: Bool -Bool -Bool
gTwo = undefined

C(gTwo) = (C(Bool) ^ C(Bool)) ^ C(Bool) = (2 ^ 2) ^ 2 = 2 ^ (2 * 2) = 2 ^ 4 = 16
```

6. Hint: 5 digit number

```haskell
fTwo :: Bool -Quad -Quad
fTwo = undefined

C(fTwo) = (C(Quad) ^ C(Quad)) ^ C(Bool) = (4 ^ 4) ^ 2 = 4 ^ (4 * 2) = 4 ^ 8 = 65536
```

## Exercises: Binary Tree

```haskell
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)
```

Given the definition of BinaryTree above, write a map function for the data structure.

```haskell
mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
        1
        (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
        2
        (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected then print "Noice"
    else error "Failed"
```

Write functions to convert BinaryTree values to lists. Make certain your implementation passes the tests.

```haskell
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node lt a rt) = a : (preorder lt) ++ (preorder rt)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node lt a rt) = (inorder lt) ++ [a] ++ (inorder rt)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node lt a rt) = (postorder lt) ++ (postorder rt) ++ [a]

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
        then putStrLn "Preorder fine!"
        else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
        then putStrLn "Inorder fine!"
        else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
        then putStrLn "Postorder fine!"
        else putStrLn "postorder failed check"

testToLists :: IO ()
testToLists = do
    testPreorder
    testInorder
    testPostorder
```

Given the definition of BinaryTree we have provided, write a catamorphism for the binary trees.

```haskell
-- any traversal order is fine
foldTree
    :: (a -> b -> b)
    -> b
    -> BinaryTree a
    -> b
foldTree _ b Leaf = b
foldTree f b (Node lt a rt) = let lb = foldTree f b lt
                                  nb = f a lb
                              in foldTree f nb rt
```
## Chapter Exercises

Multiple choice

1. Given the following datatype:

```haskell
data Weekday =
   Monday
 | Tuesday
 | Wednesday
 | Thursday
 | Friday
```

we can say:

a. Weekday is a type with five data constructors
b. Weekday is a tree with five branches
c. Weekday is a product type
d. Weekday takes five arguments

**Answer**:
a. Weekday is a type with five data constructors.

It's not a recursive datatype (and thus not a tree), it's a Sum type, not a Product type, and it takes no arguments, as both its type and all of its data constructors are Nullary.

2. and with the same datatype definition in mind, what is the type of the following function, f?

```haskell
f Friday = "Miller Time"
```

a. f :: [Char]
b. f :: String -> String
c. f :: Weekday -> String
d. f :: Day -> Beer

**Answer**:
c. f :: Weekday -> String

3. Types defined with the data keyword
a. must have at least one argument
b. must begin with a capital letter
c. must be polymorphic
d. cannot be imported from modules

**Answer**:
b. must begin with a capital letter. Datatypes can have no type arguments, can be monomorphic, and can be imported from other modules.

4. The function `g xs = xs !! (length xs - 1)`
a. is recursive and may not terminate
b. delivers the head of xs
c. delivers the final element of xs
d. has the same type as xs

**Answer**:
c. delivers the final element of `xs`. `g` is not recursive (it does not call itself), and while `xs` is of type [a], `g xs` is of type `a` and `g` itself is of type `[a] -> a`.

## As-patterns

Use as-patterns in implementing the following functions:

1. This should return True if (and only if) all the values in the first list appear in the second list, though they need not be contiguous.

```haskell
isSubseqOf
  :: (Eq a)
  => [a]
  -> [a]
  -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf aas@(a:as) (b:bs) =
  if a == b
      then isSubseqOf as bs
      else isSubseqOf aas bs
```

2. Split a sentence into words, then tuple each word with the capitalized form of each.

```haskell
capitalizeWords
  :: String
  -> [(String, String)]
capitalizeWords s = map capitalize $ words s
  where
      capitalize ccs@(c:cs) = (ccs, toUpper c : cs)
```

## Language exercises

1. Write a function that capitalizes a word.

```haskell
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs
```

2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the capitalizeWord function.

```haskell
capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . capitalize . words
  where
      capitalize [] = []
      capitalize wws@(w:ws) = capitalizeWord w : capitalizeAfterPeriod wws
      endsInPeriod [] = False
      endsInPeriod w = last w == '.'
      capitalizeAfterPeriod (wb:w:ws) =
          if endsInPeriod wb
              then capitalizeWord w : capitalizeAfterPeriod (w:ws)
              else w : capitalizeAfterPeriod (w:ws)
      capitalizeAfterPeriod _ = []
```

## Phone exercise

This exercise by Twitter user @geophf6 was originally for 1HaskellA- Day.7 Thank you for letting us use this exercise!

Remember old-fashioned phone inputs for writing text, where you had to press a button multiple times to get different letters to come up? You may still have to do this when you try to search for a movie to watch using your television remote control. You’re going to write code to translate sequences of button presses into strings and vice versa.

So! Here is the layout of the phone:

| | | |
|---|---|---|
| 1 | 2ABC | 3DEF |
| 4GHI| 5JKL | 6MNO |
| 7PQRS| 8TUV | 9WXYZ |
|  *^  | 0+_  |  #.,  |

The star (*) capitalizes the current letter, and 0 is your space bar. To represent the digit itself, you press that digit once more than the letters it represents. If you press a button one more than is required to type the digit, it wraps around to the first letter. For example:

```
2     -> 'A'
22    -> 'B'
222   -> 'C'
2222  -> '2'
22222 -> 'A'
```

1. Create a data structure that captures the phone layout above. The data structure should be able to express enough of how the layout works that you can use it to dictate the behavior of the functions in the following exercises.

```haskell
-- fill in the rest
data DaPhone = DaPhone
-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

data Button = Button Digit [Char]

newtype DaPhone = DaPhone [Button]

phone :: DaPhone
phone = DaPhone [one, two, three, four, five, six, seven, eight, nine, star, zero, pound]
  where
      one = Button '1' "1"
      two = Button '2' "abc2"
      three = Button '3' "def3"
      four = Button '4' "ghi4"
      five = Button '5' "jkl5"
      six = Button '6' "mno6"
      seven = Button '7' "pqrs7"
      eight = Button '8' "tuv8"
      nine = Button '9' "wxyz9"
      star = Button '*' "^"
      zero = Button '0' "+ 0"
      pound = Button '#' ".,#"
```

2. Convert the following conversations into the key presses required to express them. We’re going to suggest types and functions to complete, in order to accomplish the goal, but they’re not obligatory. If you want to do it differently, go right ahead:


```haskell
convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]
>
reverseTaps
  :: DaPhone
  -> Char
  -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) c =
  if isUpper c
      then ('*', 1) : taps buttons (toLower c)
      else taps buttons c
  where
      taps bs c = [buttonTaps (button bs c) c]
      buttonTaps (Button d cs) c = (d, maybe (error "boom") succ $ c `elemIndex` cs)
      button bs c = head $ dropWhile (not . includes c) $ bs
      includes c (Button _ cs) = c `elem` cs
>
cellPhonesDead
  :: DaPhone
  -> String
  -> [(Digit, Presses)]
cellPhonesDead _ [] = []
cellPhonesDead p (c:cs) = reverseTaps p c ++ cellPhonesDead p cs
```

3. How many times do digits need to be pressed for each message?

```haskell
fingerTaps
  :: [(Digit, Presses)]
  -> Presses
fingerTaps = sum . map snd
```

4. What was the most popular letter for each message? What was its cost? You’ll want to combine reverseTaps and fingerTaps to figure out what it cost in taps. reverseTaps is a list because you need to press a different button in order to get capitals.

```haskell
mostPopular :: Ord a => [a] -> a
mostPopular = head . head . reverse . sortOn length . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular

cost :: DaPhone -> Char -> Presses
cost p = fingerTaps . reverseTaps p
```

5. What was the most popular letter overall? What was the most popular word?

```haskell
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . filter (/= '\n') . unwords

coolestWord :: [String] -> String
coolestWord = mostPopular . foldMap words
```

## Hutton’s Razor

1. Your first task is to write the “eval” function which reduces an expression to a final sum.

```haskell
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add lhs rhs) = eval lhs + eval rhs
```

2. Write a printer for the expressions.

```haskell
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add lhs rhs) = printExpr lhs ++ " + " ++ printExpr rhs
```