# Exercises
## Exercises: Understanding Folds

1. b & c
2. `3 * (2 * (1 * 1))`
3. `c` - `foldr`, but not `foldl`, associates to the right.
4. a - Folds are catamorphisms, which means they are generally used to Reduce structure

### fixed:
The following are simple folds very similar to what you’ve al- ready seen, but each has at least one error. Please fix and test them in your REPL:

a.
```haskell
ghci> foldr (++) "" ["woot", "WOOT", "woot"]
"wootWOOTwoot"
```
b.
```haskell
ghci> foldr max 'a' "fear is the little death"
't'
```
c.
```haskell
-- We're not in python anymore, toto
ghci> foldr (&&) True [False, True]
False
```
d.
```haskell
-- Had to look this one up
ghci> foldr (&&) True [False, True]
False
ghci> foldr (||) True [False, True]
True
```
e.
```haskell
-- Had to look this one up..
foldl (\acc x -> acc ++ (show x)) "" [1..5]
```
f.
```haskell
ghci> foldr (flip const) 'a' [1..5]
'a'
```
g.
```haskell
ghci> foldr (flip const) 0 "tacos"
0
```
h.
```haskell
ghci> foldl const 0 "burritos"
0
```
i.
```haskell
ghci> foldl const 'z' [1..5]
'z'
```


## Database Processing

1. Write a function that filters for DbDate values and returns a list
of the UTCTime values inside them

```haskell
-- Using list comprehension
-- https://wiki.haskell.org/List_comprehension
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate ls = [ utc | DbDate utc <- ls]
```

2. Write a function that filters for DbNumber values and returns a list
of the Integer values inside them:

```haskell
-- Same as above, just with the DbNumber
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber ls = [num | DbNumber num <- ls]
```


3. Write a function that gets the most recent date

```haskell
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent ls = foldr max (head times) times
  where times = filterDbDate ls
```

4. Write a function that sums all of the DbNumber values:

```haskell
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber
```

5. Write a function that gets the average of the DbNumber values:

``` haskell
avgDb :: [DatabaseItem] -> Double
avgDb ls = (fromIntegral (sum nums)) / (fromIntegral (length nums))
   where nums = filterDbNumber ls
```

## Folding and evaluation

```haskell
fibs = 1 : scanl (+) 1 fibs

-- 1
fibsTake = take 20 fibs

-- 2
fibsLess = takeWhile (< 100) fibs

-- 3
factorial = scanl (*) 1 [1..]
```

## Warm-up and review

```haskell
-- 1
stops  = "pbtdkg"
vowels = "aeiou"

sVS = [ [s1,v,s2] | s1 <- stops, v <- vowels, s2 <- stops]

sVSP = [['p', v, s] | v <- vowels, s <- stops]

-- 2. average length of each word in a string.

-- 3
seekritFunc x = fromIntegral ws / (fromIntegral wl)
     where ws = sum (map length (words x))
           wl = length (words x)
```


## Rewriting functions using folds

1. myOr returns True if any Bool in the list is True:

```haskell
myOr :: [Bool] -> Bool
-- Same as the myAnd above this question, just with or (||)
myOr = foldr (||) False
```

2. myAny returns True if a -> Bool applied to any of the values in the
list returns True:

```haskell
myAny :: (a -> Bool) -> [a] -> Bool
myAny f ls = myOr $ map f ls
```

3. Write two versions of myElem. One version should use folding and the other should use any:

```haskell
myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (==a)
```

4. Implement myReverse. Don’t worry about trying to make it lazy:

```haskell
myReverse :: [a] -> [a]
myReverse = foldl (\b a -> a : b) []
```

5. Write myMap in terms of foldr. It should have the same behavior as the built-in map:

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []
```

6. Write myFilter in terms of foldr. It should have the same behav- ior as the built-in filter:

```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []
```

7. squish flattens a list of lists into a list:

```haskell
squish :: [[a]] -> [a]
squish = foldr (++) []
```

8. squishMap maps a function over a list and concatenates the result:

```haskell
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []
```

9. squishAgain flattens a list of lists into a list. This time, re-use the
squishMap function:

```haskell
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
```

10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returns GT for:

```haskell
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f l = foldl (\b a -> if f b a == GT then b else a)
                        (head l) l
```

11. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns LT for:

```haskell
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f l = foldl (\b a -> if f b a == LT then b else a)
                        (head l) l
```