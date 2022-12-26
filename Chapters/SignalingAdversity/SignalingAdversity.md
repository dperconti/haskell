# 12 Signaling Adversity

In Haskell it is common to use ["smart constructors"](https://wiki.haskell.org/Smart_constructors). These constructors validate their arguments and return `Maybe`, i.e. either the desired object or `Nothing` (or throw an error). For more detailed information about the error, the return type may also be Either, which holds a Left and a Right value. The former is commonly the error object.

Lifted and unlifted types have different kinds, namely * and # respectively. Lifted types are much more common and differ from unlifted types by their property of being able to be inhabited by bottom.

The type construction `[Maybe]` is invalid, because `[] :: * -> *` and `Maybe` is not `*` but `* -> *` itself.

Opposed to folds, unfolds build up data structures from a single starting value (anamorphism). `iterate :: (a -> a) -> a -> [a]` does that infinitely, `unfoldr :: (b -> Maybe (a, b)) -> b -> [a]` (in [Data.List](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html#v:unfoldr)) is the generalization which may terminate.