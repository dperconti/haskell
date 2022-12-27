# 8 Recursion
---

A **recursive function** is defined in terms of itself. The **base case** ends the recursion, e.g. factorial of 0 is 1.

In Haskell, **bottom** is a _non-value_ that is used to indicate that a function can not return a value. Possible reasons are errors, partial functions, or infinite recursion / loops.

An example for an elegantly formulated recursive function, performing an integral division:

```haskell
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
```