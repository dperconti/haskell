# 10 Folding Lists
Folding is the reduction of a structure. It happens at two stages, namely (1) traversal and (2) reduction. Folding, as a concept, is also refered to as **catamorphism**, that is the unique homomorphism (structure preserving map) from an initial algebra into some other algebra.

The right associative function **fold right**, `foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`, applies a base value and the last value of a foldable type to a function, takes the result and recursively applies the function to a sequence of values, yielding one value as its final result. The function folds a foldable type with the function `f :: a -> b -> b`.

When computing the product of all values of a foldable, the base value (identity) is 1; for sums it would be 0. The identity is also returned, if the foldable data structure contains no value, e.g. an empty list `[]`.

The **left fold** is traversing the data structure in the same order as the right fold, however it is left associative. It is inappropriate to use in combinations with very long lists or impossible with infinite lists. `foldl'` is the strict version of foldl. The relationship between `foldl` and `foldr` is (for finite lists `xs`) `foldr f z xs = foldl (flip f) z (reverse xs)`.

**Scans** return a list of all intermediate values of a fold. `scanr :: (a -> b -> b) -> b -> [a] -> [b]` and `scanl` are the Haskell function for right fold and left fold respectively. `scanl` can for example be used to create an infinite list of Fibonacci numbers: `fibs = 1 : scanl (+) 1 fibs`.


-   One initially non-obvious aspect of folding is that it happens in two stages, `traversal` and `folding`.
    -   `traversal` is the stage in which the fold recurses over the spine.
    -   `folding` refers to the evaluation or reduction of the folding function applied to the values.
-   Folds recurse over the spine in the same direction; the difference is in the association, or parenthesization, of the folding function.

-   `Foldr` can avoid evaluating not just some or all of the values in the list, but some or all of the list's spine as well. For this reason, foldr can be used with lists that are potentially infinite.

-   In `Foldl`, recursion of the spine is unconditional. This feature means that foldl is generally inappropriate with lists that are or could be infinite; it is also usually inappro- priate even for long lists, as it accumulates a pile of unevaluated values as it traverses the spine.

-   In most cases, when you need a left fold, you should use foldl', which works the same except it is strict.

-   For finite lists, `foldr f z xs = foldl (flip f) z (reverse xs)`

-   A way to write fibs using scan: `fibs = 1 : scanl (+) 1 fibs`


Copied from [this blog](https://tgdwyer.github.io/haskell4/)


Recall the `reduce` function that is a member of JavaScript’s `Array` type, and which we implemented ourselves for linked and cons lists, was a way to generalise loops over enumerable types. In Haskell, this concept is once again generalised with a typeclass called Foldable – the class of things which can be “folded” over to produce a single value.
We will come back to the `Foldable` `typeclass`, but first let’s limit our conversation to the familiar `Foldable` instance, basic lists.
Although in JavaScript `reduce` always associates elements from left to right, Haskell’s `Foldable` typeclass offers both foldl (which folds left-to-right) and foldr (which folds right-to-left):

```haskell
Prelude> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

Prelude> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

In the following the examples the `Foldable t` instance is a list. Here’s how we right-fold over a list to sum its elements:


![Alt text](https://tgdwyer.github.io/assets/images/chapterImages/haskell4/rightFold.png)


While the lambda above makes it explicit which parameter is the accumulator and which is the list element, this is a classic example where point-free coding style makes this expression very succinct:

```haskell
Prelude> foldr (+) 0 [5,8,3,1,7,6,2]
32
```

Here’s a left fold with a picture of the fold:

![Alt text](https://tgdwyer.github.io/assets/images/chapterImages/haskell4/leftFold.png)


Note that since the `(+)` operator is associative – a+(b+c) = (a+b)+c – `foldr` and `foldl` return the same result. For functions that are not associative, however, this is not necessarily the case.