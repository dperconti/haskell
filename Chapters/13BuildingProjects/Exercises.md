# Chapter 13 Exercises

## Intermission: Check your understanding

For our purposes right now, it does not matter whether you are familiar with the modules referenced in the import list. Look at the declarations and answer the questions below:

1. What functions are being imported from Control.Monad?
- forever
- when

2. Which imports are both unqualified and imported in their entirety?
- Data.Bits
- Database.Blacktip.Types

3. From the name, what do you suppose importing the Types module brings in?
- datatypes defined for the blacktips library

4. Now, let’s compare a small part of blacktip’s code to the above import list:
```haskell

writeTimestamp :: MV.MVar ServerState
               -> FPC.FilePath
               -> IO CC.ThreadId

writeTimestamp s path = do
  CC.forkIO go
  where go = forever $ do
          ss <- MV.readMVar s mask $ \_ -> do
            FS.writeFile path
            (B.pack (show (ssTime ss)))
          -- sleep for 1 second
          CC.threadDelay 1000000
```

a. The type signature refers to three aliased imports. The modules named in those aliases are: - Control.Concurrent.MVar - Filesystem.Path.CurrentOS - Control.Concurrent
b. FS.writeFile refers to Filesystem import.
c. forever comes from Control.Monad import.

## Modifying Code

2. Here is a very simple, short block of code. Notice it has a forever that will make it keep running, over and over again. Load it into your REPL, and test it out. Then, refer back to the chapter, and modify it to exit successfully after a False result:

```haskell
import Control.Monad

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"
```

```haskell
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
```

3. If you try using palindrome on a sentence such as “Madam I’m Adam,” you may notice that it doesn’t work. Modifying the above so that it works on sentences, too, involves several steps. You may need to refer back to previous examples in the chapter to get ideas for proper ordering and nesting. You may wish to import Data.Char to use the function toLower. Have fun.

```haskell
palindrome2 :: IO ()
palindrome2 = forever $ do
  line1 <- getLine
  let line2 = map toLower . filter isLetter $ line1
  case (line2 == reverse line2) of
    True -> putStrLn "It's a palindrome!"
    False ->putStrLn "Nope!"
```

4.

```haskell
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
 hSetBuffering stdout NoBuffering
 putStr "Name:"
 name <- getLine
 putStr "Age:"
 age <- fmap read getLine
 let person = mkPerson name age
 case person of
    Right p -> putStrLn $ "Successfully retrieved person: " ++ show p
    Left error -> putStrLn $ "Error: " ++ show error
```