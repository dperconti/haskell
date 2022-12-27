# 2 Getting Started
---

*Prelude* is a library of standard types, classes, and functions, such as `pi`, `Bool`, `Monad`, `map`. Haskell files can be loaded to GHCi REPL using `:load file.hs`.

An expression is in normal form, or irreducible, when there are no more evaluations steps that can be taken.

Every Haskell function is an expression that takes one argument. They always return a result. A definition may look like that:

```haskell
piTimesSquare x = pi * (x ^ 2).
```

A *function parameter* stands for a value, while an argument is an actual value that is being passed on to the function. Functions are in prefix style by default.

*Functions* in Haskell are usually called using prefix notation, or the function name followed by its arguments. However, some functions e.g. addition are called using *infix notation* - putting the function name between its two arguments:

```haskell
Prelude> 17 + 25
 42
```

*Infix operators* are functions that can be used in prefix fashion by wrapping them in parentheses: `(+) 1 2`. The $ operator has the lowest possible precedence (0). The following example explains its usage: (5 *) $ 1 + 1 equals 5 * (1 + 1). The GHCi command info provides signature and precedence information about functions.

*Infix operators*: Putting `-marks around a prefix function allows us to use it like an infix function:

```haskell
 Prelude> let concatPrint x y = putStrLn $ (++) x y
 Prelude> concatPrint "a" "b"
 ab
 Prelude> "a" `concatPrint` "b"
 ab
```

Note that you can only normally do this with a function that takes two arguments. Actually, for a function taking more than two arguments, you can do it but it's not nearly as nice (note the need for extra parentheses):

```haskell
Prelude> foldl (+) 0 [1..5]
15
Prelude> ((+) `foldl` 0) [1..5]
15
```

An *expression* is a combination of symbols that conforms to syntactic rules and can be evaluated to some result.

A *value* is an expression that can not be evaluated any further. Haskell uses lazy evaluation, i.e. it only evaluates an expression when it is forced to by other terms which refer to the expression.