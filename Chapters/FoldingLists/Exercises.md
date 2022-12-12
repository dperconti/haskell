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
