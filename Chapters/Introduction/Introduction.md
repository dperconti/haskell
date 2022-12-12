# 1 Introduction
___

A *function* maps from its domain to its image (which is a subset of the co-domain). Each input is invariably mapped to exactly *one* output.

The most basic way of defining a function in Haskell is to 'declare' what it does. For example, we can write:

```
   double :: Int -> Int
   double n = 2*n
```

Here, the first line specifies the type of the function and the second line tells us how the output of double depends on its input. We shall see soon that the 'definition' of double is computed by treating the equality as a rule for rewriting expressions. We shall also see that, in Haskell, the type of a function can be inferred automatically, so we can omit the type when defining a function.

---

In *lambda calculus* an abstraction is an anonymous function. It consists of head and body, for example `λx.x`. The head binds the parameter(s) to the body of the function.

The lambdas `λx.x` and `λy.y` are alpha equivalent.

*Beta reduction* is the process of replacing all occurrences of a parameter with a value or a function; for example `(λx.x + x)1` becomes `1` or `(λx.x)(λa.2a)` turns into `(λa.2a)`.

If a variable occurs in a function’s body, but not in the head, it is referred to as a free variable. Lambdas with multiple arguments such as `λxy.xy` are a shorthand for multiple nested lambdas `λx.(λy.xy)`.

*Combinators* are lambda terms with no free variables.

Lambda terms can diverge if evaluation does not terminate. For example `λx.xx` diverges if applied to itself. Evaluation happens in normal order, i.e. outer-most and left-most terms get evaluated first.

Notes on syntax: `λab.a(b)` means that b will be applied to a on evaluation (if possible). However, `(λa.λb.a)b` evaluates to `λb.b`.