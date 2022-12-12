

Extra Load
----------

``` haskell
import Data.Char
```

Exercises
---------

**Exercises: EnumFromTo**

Write your own `enumFromTo` definitions for the types provided. Do not use range syntax to do so. It should return the same results as if you did `[start..stop]`.

```haskell
main :: IO ()

enumFromToDonato :: (Ord a, Enum a) => a -> a -> [a]
enumFromToDonato from to
    | from >  to = []
    | from == to = [from]
    | from <  to = from : enumFromToDonato (succ from) to

main = print (enumFromToDonato 1 10)
```

**Exercises: Thy Fearful Symmetry**

```haskell
main :: IO ()

donatoWords :: String -> [String]
donatoWords [] = []
donatoWords s = current : donatoWords rest
  where current = takeWhile (/= ' ') s                      -- Basically saying if the current string is NOT an empty space, 'take' it
        rest = dropWhile (== ' ') . dropWhile (/= ' ') $ s  -- Basically saying it the current string IS an empty space, 'drop' it

main = print (donatoWords "sheryl wants fun")


❯ runhaskell TypeTest.hs
["sheryl","wants","fun"]
```

### My Lines
```haskell
donatoLines :: String -> [String]
donatoLines [] = []
donatoLines s = current : donatoLines rest
  where current = takeWhile (/= '\n') s
        rest = dropWhile (== '\n') . dropWhile (/= '\n') $ s
```

### My Break
```haskell
main :: IO ()

donatoBreak :: Char -> String -> [String]
donatoBreak _ [] = []
donatoBreak c s = current : donatoBreak c rest
  where current = takeWhile (/= c) s
        rest = dropWhile (== c) . dropWhile (/= c) $ s

main = print (donatoZipWith (==) ['a'..'f'] ['a'..'m'])


```

**Exercises: Bottom Madness**

1. no `callstack error`
2. yes
3. no
4. yes
5. no
6. yes
7. no
8. yes
9. yes
10. no
11. whnf. nf
12. whnf
13. neither
14. neither
15. neither
16. neither
17. whnf

**Exercises: More Bottoms**

1.  bottom
2.  2
3.  bottom
4.  return a list of boolean which indicates the character is in "aeiou" or not.
5.  \[1,4,9,16,25,36,49,64,81,100\]; \[1,10,20\]; \[15,15,15\]
6.  `map (\x -> bool x (-x) (x == 3)) [1..10]`

**Exercises: Filtering**

``` haskell
-- 1.
filterThree = filter (\x -> mod x 3 == 0)

-- 2.
lengthThree = length . filter (\x -> mod x 3 == 0)

-- 3.
removeArticles ls = [x | x <- words ls, x/="a", x/="an", x/="the"]
```

**Zipping exercises**

``` haskell
-- 1. Write your own version of zip, and ensure it behaves the same as the original
main :: IO ()

donatoZip :: [a] -> [b] -> [(a, b)]
-- Defaults for empty lists
donatoZip [] _ = []
donatoZip _ [] = []
-- zip everything together...
donatoZip (x:xs) (y:ys) = (x, y): donatoZip xs ys

main = print (donatoZip [1,2,3] [4,5,6])
```

```haskell

-- 2. Do what you did for zip but now for zipWith:
main :: IO ()

donatoZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
donatoZipWith _ [] _ = []
donatoZipWith _ _ [] = []
-- Just gotta add the function
donatoZipWith f (x:xs) (y:ys) = f x y: donatoZipWith f xs ys

main = print (donatoZipWith (==) ['a'..'f'] ['a'..'m'])
```

```haskell
-- 3. Rewrite your zip in terms of the zipWith you wrote.
-- I think this is what they're asking for?
donatoZip2 = donatoZipWith (\x y -> (x, y))
```

Chapter Exercises
-----------------

**Data.Char**

``` haskell
-- 1. Query the types of isUpper and toUpper.
ghci> import Data.Char
ghci> :t isUpper
isUpper :: Char -> Bool
ghci> :t toUpper
toUpper :: Char -> Char
ghci>

-- 2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a String? Write that function such that, given the input "HbEfLrLxO", your function will return "HELLO".
-- Just filter it...
filterUpper = filter isUpper

-- 3. Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument "julie", it will return "Julie".
import Data.Char

main :: IO ()

capitalizeString [] = []
-- Reminder that `:` is the "prepend" operator
capitalizeString (x:xs) = toUpper x : xs

main = print (capitalizeString "julie")

-- ❯ runhaskell TypeTest.hs
-- "Julie"
```

```haskell

-- 4. Now make a new version of that function that is recursive, such that if you give it the input "woot", it will holler back at you "WOOT". The type signature won’t change, but you will want to add a base case.

import Data.Char

main :: IO ()

-- base case
capitalizeStringRec [] = []
-- yeet
capitalizeStringRec (x:xs) = toUpper x : capitalizeStringRec xs

capitalizeHead = toUpper. head

main = print (capitalizeStringRec "julie")

-- ❯ runhaskell TypeTest.hs
-- "JULIE"
```

**Ciphers**

``` haskell
caesarChar num char = chr (ord 'a' + shift)
   where distance = ord char - ord 'a'
         shift = (distance + num) `mod` 26

caesar num str = map (caesarChar num) str

unCaesar num str = map (caesarChar (26 - (mod num 26))) str
```

**Writing your own standard functions**

``` haskell
-- 1 myOr returns True if any Bool in the list is True:
import Data.Char

main :: IO ()

myOr :: [Bool] -> Bool
-- Better version I found online
-- myOr [] = False
-- myOr (x:xs) = x || myOr xs

myOr [] = False
myOr (x:xs) = if x then True
                   else myOr xs

-- main = print ((myOr [True, True, True, True, True, True]) == True)
main = print (myOr [False, True, True, True, True, False, True] == True)
```

```haskell
-- 2 myAny returns True if a -> Bool applied to any of the values in the list returns True:
import Data.Char

main :: IO ()

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

-- Old version
-- myAny :: (a -> Bool) -> [a] -> Bool
-- myAny _ [] = False
-- myAny f (x:xs) = if f x then True
--                        else myAny f xs
-- Prelude> myAny even [1, 3, 5]
-- False
-- Prelude> myAny odd [1, 3, 5]
-- True

main = print (myAny even [1, 3, 5] == False)

```


```haskell

-- 3 After you write the recursive myElem, write another version that uses any. The built-in version of elem in GHC 7.10 and newer has a type that uses Foldable instead of the list type, specifically. You can ignore that and write the concrete version that works only for lists:
main :: IO ()

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

-- myElem2 :: Eq a => a -> [a] -> Bool
-- myElem2 e xs = myAny (==e) xs

main = print (myElem 1 [1..10])

```

```haskell
-- 4 Implement myReverse:
main :: IO ()

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

main = print ((myReverse "shenanigans") == "snaginanehs")

```

```haskell
-- 5 squish flattens a list of lists into a list:
main :: IO ()

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

main = print ((squish ["shenanigans", "smashing"]) == "shenaniganssmashing")
```

```haskell
-- 6  squishMap maps a function over a list and concatenates the results
main :: IO ()

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

main = print ((squishMap (\x -> [1, x, 3]) [2]) == [1,2,3])
```

```haskell

-- 7 squishAgain flattens a list of lists into a list. This time, re-use the squishMap function:
main :: IO ()

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- main = print ((squishMap (\x -> [1, x, 3]) [2]) == [1,2,3])

-- 8 myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returns GT for. If you import maximumBy from Data.List, you’ll see that the type is:
main :: IO ()

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = go comp xs x
 where go _ [] cur = cur
       go comp (x:xs) cur = go comp xs (if comp cur x == GT then cur else x)

-- main = print ((squishMap (\x -> [1, x, 3]) [2]) == [1,2,3])
```


```haskell
-- 9 myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns LT for:

main :: IO ()

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = go comp xs x
 where go _ [] cur = cur
       go comp (x:xs) cur = go comp xs (if comp cur x == LT then cur else x)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

-- main = print ((squishMap (\x -> [1, x, 3]) [2]) == [1,2,3])

```