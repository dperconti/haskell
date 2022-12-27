# Exercises

2. If the function requires a Mood value, what are the values you could possibly use?

```haskell
data Mood = Blah | Woot deriving Show
```

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

```haskell
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah
```


1. `length :: Foldable t => t a -> Int`
2. 5; 3; 3; 4
3. `6 / length [1, 2, 3]`
4. should use `div`
5. Bool; True
6. Bool; False
7. True; not work; 5; False; not work
8.
``` haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x
```
9.
``` haskell
myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else -x
```
10.
``` haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
let f x y = ((snd x, snd y), (fst x, fst y))
```

### Correcting syntax

1.
```haskell
x = (+)
f xs = w `x` 1
where w = length xs
```
2.
``` haskell
\x=x
```
3.
``` haskell
\(x : xs) -> x
```
4.
``` haskell
f (a, b) = a
```

### Match the function names to their types

1. c
2. b
3. a
4. d


1. a
2. b
3. a
4. D
