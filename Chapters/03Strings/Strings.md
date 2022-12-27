# 3 Strings
---

The GHCi command `:type` prints the type of a variable / expression. `a :: b` means that a has the type `b`.

`String` is a type alias for `[Char]`, i.e. a list of characters.

For outputting variables, `print` can be used. `putStr` and `putStrLn` are also printing, however, they are restricted to the type String.

```haskell
a :: String  -- declaration with type
a = "a"  -- value assignment

main :: IO ()
main = do
  putStr a
  putStrLn "b"
```

```haskell
ghci> a :: String  -- declaration with type
"a"
ghci> a = "a"  -- value assignment
ghci>
ghci> putStr a
aghci> putStrLn "b"
b
```

Strings can be concatenated with the infix operator `++` or the concat function (e.g. `concat ["a", "b"]`).

Functions and types can be defined globally (top level definitions) or locally (local definition); the scope is different. The were and let clauses are key to defining local functions or variables.

```haskell
area d = pi * (r * r)  -- top level
  where r = d / 2  -- local
```

The `:` operator builds a list: `'a' : "bc"`. The functions head and tail can be applied to strings in order to retrieve the first character (`head`) or everything but the first character (`tail`). A substring starting at index 0 can be retrieved using take: take `n` string. It will return a list containing the first `n` elements of the list (which can be a String). Contrary, drop removes the first n elements from a list.

```haskell
-- sub list with length l of list x starting at index s
-- pointfree version: https://timodenk.com/blog/making-slice-pointfree/
slice :: Int -> Int -> [a] -> [a]
slice s l x = take l (drop s x)
```