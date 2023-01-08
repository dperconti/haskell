# 16.10 Exercises: Instances of Func

Implement Functor instances for the following datatypes. Use the QuickCheck properties we showed you to validate them:

1. newtype Identity a = Identity a
2. data Pair a = Pair a a
3. data Two a b = Two a b
4. data Three a b c = Three a b c
5. data Three' a b = Three' a b b
6. data Four a b c d = Four a b c d
7. data Four' a b = Four' a a a b
8. Can you implement one for this type? Why? Why not?
     data Trivial = Trivial

Doing these exercises is critical to understanding how Functor works—do not skip past them!

```haskell

import Test.QuickCheck (quickCheck, Arbitrary(..))
import Test.QuickCheck.Function (Fun(..))

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorComposition (Fun _ f) (Fun _ g) x
  = (fmap g (fmap f x)) == (fmap (g . f) x)

type IntToChar = Fun Int Char
type CharToBool = Fun Char Bool
```

1. newtype Identity a = Identity a

```haskell
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityTest = Identity Int
```

2. data Pair a = Pair a a

```haskell

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

type PairTest = Pair Int
```

3. data Two a b = Two a b

```haskell
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoTest = Two Char Int
```

4. data Three a b c = Three a b c

```haskell
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeTest = Three Bool Char Int
```

5. data Three' a b = Three' a b b

```haskell
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

type Three'Test = Three' Char Int
```

6. data Four a b c d = Four a b c d

```haskell
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

type FourTest = Four Bool String Char Int
```

7. data Four' a b = Four' a a a b

```haskell
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
      a1 <- arbitrary
      a2 <- arbitrary
      a3 <- arbitrary
      b <- arbitrary
      return $ Four' a1 a2 a3 b

type Four'Test = Four' Char Int
```

8. Can you implement one for this type? Why? Why not?

```haskell

data Trivial = Trivial

```

> I don't think so because it's not not * -> *?


```haskell
-- Tests
test :: IO ()
test = do
  quickCheck (functorIdentity :: IdentityTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> IdentityTest -> Bool)
  quickCheck (functorIdentity :: PairTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> PairTest -> Bool)
  quickCheck (functorIdentity :: TwoTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> TwoTest -> Bool)
  quickCheck (functorIdentity :: ThreeTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> ThreeTest -> Bool)
  quickCheck (functorIdentity :: Three'Test -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> Three'Test -> Bool)
  quickCheck (functorIdentity :: FourTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> FourTest -> Bool)
  quickCheck (functorIdentity :: Four'Test -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> Four'Test -> Bool)
```


# 16.17 Chapter exercises

```haskell
{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr (Array)
```


Determine whether a valid Functor can be written for the datatype provided:

1. `data Bool = False | True`
> No, because it is not * -> *

2. `data BoolAndSomethingElse a = False' a | True' a`

```haskell
data BoolAndSomethingElse a = False' a | True' a
  deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True'  (f a)
```


3. `data BoolAndMaybeSomethingElse a = Falsish | Truish a`

```haskell
data BoolAndMaybeSomethingElse a = Falsish | Truish a
  deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)
```

4. Use the kinds to guide you on this one—don’t get too hung up
on the details:
`newtype Mu f = InF { outF :: f (Mu f) }`

what...?
Came across [this stackoverflow](https://stackoverflow.com/questions/39770191/functor-instance-for-newtype-mu-f-inf-outf-f-mu-f).

> You can't. In order to define an instance Functor c for some c, c must be of the kind * -> *. So in your case, Mu should have been of that kind, which means that its argument f must have been of the kind *. But clearly this is not the case, as you are applying f to something else (to Mu f).

😵‍💫

5. Again, follow the kinds, and ignore the unfamiliar parts:
```haskell
import GHC.Arr data D = D (Array Word Word) Int Int
```

Again, I don't think you can. Kind is * and therefore no functor.


### Rearrange the arguments to the type constructor of the datatype so the Functor instance works:

1.
Original
```haskell
data Sum a b =
  First a
  | Second b
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b
```

Solution
```haskell
data Sum b a = First a | Second b
  deriving Show

instance Functor (Sum e) where
  fmap _ (Second b) = Second b
  fmap f (First  a) = First (f a)
```

2.
Original
```haskell
data Company a b c =
  DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
```

Solution
```haskell
data Company a c b = DeepBlue a c | Something b
  deriving Show

instance Functor (Company e e') where
  fmap _ (DeepBlue a c) = DeepBlue a c
  fmap f (Something b)  = Something (f b)
```

3.
Original
```haskell
data More a b =
  L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
```

Keeping in mind that it should result in a Functor that does the following:
```haskell
Prelude> fmap (+1) (L 1 2 3) L224
Prelude> fmap (+1) (R 1 2 3) R133
```

```haskell
data More b a = L a b a | R b a b
  deriving Show

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
```

Solution

```haskell
data More b a = L a b a | R b a b
  deriving Show

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
```

### Write Functor instances for the following datatypes:

1.
```haskell
-- Is this a typo?? what is a Bloor?
data Quant a b = Finance
  | Desk a
  | Floor b
```

```haskell
data Quant a b = Finance | Desk a | Floor b
  deriving Show

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Floor b) = Floor (f b)
```

2. No, it’s not interesting by itself:
```haskell
data K a b = Ka
```

```haskell
data K a b = K a
  deriving Show

instance Functor (K a) where
  fmap _ (K a) = K a
```

3.

```haskell
{-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = Ka

-- This should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap = undefined
```

```haskell
newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a
  deriving Show

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))
```

4.
```haskell
data EvilGoateeConst a b =
  GoatyConst b
-- You thought you'd escaped the goats
-- by now didn't you? Nope.
```

No, it doesn’t do anything interesting. No magic here or in the
previous exercise. If it works, you succeeded.

```haskell
data EvilGoateeConst a b = GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
```

5. Do you need something extra to make the instance work?
```haskell
data LiftItOut f a =
  LiftItOut (f a)
```

```haskell
data LiftItOut f a = LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
```

6.
```haskell
data Parappa f g a = DaWrappa (f a) (g a)
```

```haskell
data Parappa f g a = DaWrappa (f a) (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
```

7. Don’t ask for more type class instances than you need. You can let GHC tell you what to do:
```haskell
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
```

```haskell
data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
  deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

```


8.
```haskell
data Notorious g o a t = Notorious (g o) (g a) (g t)
```


```haskell
data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
```

9. You’ll need to use recursion:
```haskell
data List a = Nil
  | Cons a (List a)
```

```haskell
data List a = Nil | Cons a (List a)
  deriving Show

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)
```

10. A tree of goats forms the Goat-Lord, a fearsome poly-creature:
```haskell
data GoatLord a = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
-- A VERITABLE HYDRA OF GOATS
```

```haskell
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving Show

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)
```

11. You’ll use an extra functor for this one, although your solution might do it monomorphically without using fmap. Keep in mind that you will probably not be able to validate this one in the usual manner. Do your best to make it work:

```haskell

data TalkToMe a = Halt
  | Print String a
  | Read (String -> a)
```


```haskell
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt         = Halt
  fmap f (Print s a)  = Print s (f a)
  fmap f (Read sa)    = Read (\s -> f (sa s))
```