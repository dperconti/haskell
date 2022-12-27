# 9 Lists

In Haskell, lists are:
1. a **collection of elements** of the same type, or
2. **an infinite series** of values (i.e. stream).

The **list definition** is data `[] a = [] | a : [a]`.

The initialization `[1, 2, 3] ++ [4]` is syntactic sugar for `(1 : 2 : 3 : []) ++ 4 : []`. The range syntax allows for the definition of sequences from _n_ to _m_ with `[n..m]` and a step size of 1. It uses enumFromTo behind the scenes; and enumFromThenTo works for variable step sizes.

**List comprehensions** are a means of generating a new list from an existing list (or multiple lists). For instance `[sqrt x | x <- [0..10], sqrt x < 3]` generates a list of square roots of the numbers from 0 to 10, for the cases where the square root is smaller than 3. `x <- [0..10]` is called generator. Multiple generators can be used to create a new list, e.g. `[x*y | x <- [0..10], y <- [10..12]]`. In such a case, each element of the first list will be processed with every element of the second, and so forth.

In the case of a list, the spine is a linear succession of one cons cell wrapping another cons cell `(1 : 2 : 3 : [])`. Spines are evaluated independently of values. Here, the spine is the structure of a collection, i.e. not the values contained therein. Calling the `length` function with a list does not necessarily lead to an evaluation of all values. The `sprint` command (which is a GHCi feature, not part of the Haskell language) allows you to see how much of a value has been evaluated at this [point](https://stackoverflow.com/a/35200329/3607984).

Values in Haskell get reduced to *weak head normal form** by default. Normal form means that an expression is fully evaluated. Weak head normal form means the expression is only evaluated as far as is necessary to reach a data constructor. `"a" ++ "b"` is neither of both because the outermost component of the expression is a function.

## 9.1 List Utility Functions
- `!!` returns the nth element
- `take` returns the first n elements of a list. `take :: Int -> [a] -> [a]`
- `drop` returns all but the first n elements of a list. `drop :: Int -> [a] -> [a]`
- `takeWhile` iterates over the list and returns all elements until the condition mismatches. `takeWhile :: (a -> Bool) -> [a] -> [a]`
- `dropWhile` iterates over the list and discards all elements until the condition mismatches. `dropWhile :: (a -> Bool) -> [a] -> [a]`
- `splitAt` returns a tuple containing the first n and the remaining elements of the list. `splitAt :: Int -> [a] -> ([a], [a])`
- `head` returns the first element of a list. If the list is empty, and exception is thrown.
- `last` returns the last element of a list. Throws an exception if the list is empty.
- `tail` returns all elements but the first (head). If the list is empty, an exception is thrown.
- `init` returns all elements but the last. Throws an exception if the list is empty.
- `elem` checks whether an element is in a list or not. `elem :: (Eq a, Foldable t) => a -> t a -> Bool`
- `map` applies a function to all elements. `map :: (a -> b) -> [a] -> [b]`
- `zip` creates a list of tuples out of two lists. It stops as soon as one list runs out of values. `zip :: [a] -> [b] -> [(a, b)]`
- `unzip` creates a tuple of two lists out of a list of tuples. `unzip :: [(a, b)] -> ([a], [b])`
- `zipWith` combines two lists into one by subsequently applying a function to two elements. `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`