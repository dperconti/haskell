# Strings Exercises

### Reading syntax
1. For the following lines of code, read the syntax carefully, and decide whether they are written correctly. Test them in your REPL in order to check your work. Correct as many as you can:
    1. `concat [[1, 2, 3], [4, 5, 6]]`
    2. `(++) [1, 2, 3] [4, 5, 6]`
    3. `(++) "hello" " world"`
    4. `["hello" ++ " world"]`
    5. `"hello" !! 4`
    6. `(!!) "hello" 4`
    7. `take 4 "lovely"`
    8. `take 3 "awesome"`

2.  Next, we have two sets. The first set is lines of code, and the other is a set of results. Read the code, and figure out which results come from which lines of code. Be sure to test them in the REPL. Match each of the previous expressions to one of these results, which are presented in a scrambled order:
    1.  a -> d
    2.  b -> c
    3.  c -> e
    4.  d -> a
    5.  e -> b

### Building functions

 1. Given the list-manipulation functions mentioned in this chap- ter, write functions that take the given inputs and return the expected outputs. Do them directly in your REPL, and use the take and drop functions you’ve already seen. Write expressions to perform the following transforma- tions, just with the functions you’ve seen in this chapter. You do not need to do anything clever here:
    1. `"Curry is awesome" ++ "!"`
    2. `drop 4 (take 5 "Curry is awesome!")`
    3. `drop 9 "Curry is awesome!"`

2.

```haskell
thirdLetter :: String -> Char
thirdLetter x = drop 2 (take 3 x)
```

4.

```haskell
letterIndex :: Int -> Char
letterIndex x = drop (x-1) (take x "Curry is awesome!" )
```

5.

```haskell
rvrs :: String -> String
rvrs s = (drop 9 s) ++ (take 4 (drop 5 s)) ++ (take 5 s) ++ "."
```

6.
```haskell
module Reverse where

rvrs :: String -> String
rvrs x = (drop 9 s) ++ (take 4 (drop 5 s)) ++ (take 5 s) ++ "."

main :: IO ()
main = print $ rvrs "Curry is awesome"
```