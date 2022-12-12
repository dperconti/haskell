# 7 Functional Patterns
---

Inner variables can _shadow_ outer variables, as can bee seen in the following function which always returns `5`: `func x = let x = 5 in x`.

**Anonymous functions** (aka. lambdas) are functions which are not bound to an identifier and can be declared with this syntax: `(\x -> x * 4) :: Num a => a -> a`. They are often used if a function is passed to another function with the former being needed only once. The signature can be omitted.

The signature of **higher order functions** contains functions itself. For example `distributor :: (a -> b -> c) -> (a -> b) -> (a -> c)` takes two functions and returns a new one.

The **guard syntax** of Haskell allows to write compact functions with multiple outcomes depending on boolean conditions. Each line behind a pipe is called guard case. `otherwise` is a constant that equals `True`, i.e. the catch-all case.

```haskell
clip :: (Num a, Ord a) => a -> a -> a -> a
clip min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x
```

**Pointfree** versions of functions drop arguments for the sake of readability and performance. For example, `print a = (putStrLn . show) a` becomes `print = putStrLn . show`.

**Binding** is the assignment of an argument to a parameter.