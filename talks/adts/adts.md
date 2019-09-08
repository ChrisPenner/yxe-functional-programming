build-lists: true
theme: Poster, 1
slide-transition: push(horizontal)
text: #FF5481
header: #FF5481
text-emphasis: #FFFFFF
text-strong: #FF5481

# ADTs

## Algebraic Data Types

---

#[fit] What are they?

---

#[fit] You've seen them before

---

#[fit] `data Point = Point Int Int`

---

## Creating

```haskell
data Point = Point Int Int
  deriving Show

>>> origin = Point 0 0

>>> origin
Point 0 0
```

---

# *ADTs* have a 
#[fit]**SHAPE**

---

## Unpacking

```haskell
getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y

>>> point = Point 10 35
>>> getX point
10
>>> getY point
35
```

---

## Modifying

```haskell
moveX :: Int -> Point -> Point
moveX n (Point x y) = Point (x + n) y

>>> point
Point 10 35

>>> moveX 5 point
Point 15 35
```

---

# *ADTs can have*

## **Multiple Constructors**

---

##[fit] You've seen this before too...

#[fit] Any *guesses*?

---

#[fit] `data Bool = True | False`

---

```haskell
>>> :t True
True :: Bool

>>> :t False
False :: Bool
```

---

#[fit]This is

#[fit]**_literally_**

#[fit]How it's defined in Haskell!

---

# Let's write: `if`

```haskell
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
```

---

## Ternary anyone?

```haskell
(?) :: Bool -> a -> a -> a
(?) = if'
```

It's not a broken slide... that's actually it...

```haskell
>>> (3 == 3) ? "Eureka!" $ "Excelsior!"
"Eureka!"

>>> (3 == 10) ? print "FALSE!" $ getLine >>= putStrLn . reverse
> hello
olleh
```

---

Where were we again?

---

Oh right...

---

# Case Statements

---

## Case statement

```haskell
if'' :: Bool -> a -> a -> a
if'' theBool whenTrue whenFalse =
    case theBool of
        True  -> whenTrue
        False -> whenFalse
```

---


#[fit] *ADTs* are 

#[fit] **primitives**

---

#[fit] ...

---

## Records

```haskell
data Point' =
    Point' { x :: Int
           , y :: Int
           } deriving Show


>>> point = Point' 3 7
>>> point
Point' {x = 3, y = 7}
```

---

# Getters

```haskell
>>> point
Point' {x = 3, y = 7}

>>> x point
3
```

---

# Setters

```haskell
>>> point
Point' {x = 3, y = 7}

>>> point{ y = 27 }
Point' {x = 3, y = 27}
```

---

# Unpacking Records

```haskell
distance :: Point' -> Point' -> Double
distance Point' {x = x1, y = y1} Point' {x = x2, y = y2} =
    sqrt(fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))
```

#### You can do all the *javascript-y* stuff too...

---

#[fit] We can **_derive_** things!

---

```haskell
data Point = Point Int Int 
  deriving (Show, Eq, Ord)
```

---

# **REAL** equaity

```haskell
>>> Point 3 10 == Point 3 10
True

>>> point1 = Point 1 2
>>> point2 = Point 1 2
>>> point1 == point2
True
```

---

```haskell
>>> Point 3 5 < Point 3 6
True

>>> Point 3 5 < Point 3 2
False

>>> Point 3 5 < Point 0 2
False
```

---

```haskell
data AccountStatus = 
    Active
  | Pending
  | Paused
  | Cancelled
  | Disabled
    deriving (Show, Eq, Ord, Enum, Bounded)

>>> enumFrom Active
[Active,Pending,Paused,Cancelled,Disabled]

>>> [Active ..]
[Active, Pending, Paused, Cancelled, Disabled]

>>> [minBound .. maxBound ] :: [AccountStatus]
[Active, Pending, Paused, Cancelled, Disabled]
```

---

## Enum Tricks

```haskell
>>> ['a' .. 'z']
"abcdefghijklmnopqrstuvwxyz"

>>> [10, 8 .. 0]
[10,8,6,4,2,0]
```

---

We can derive other things too!

* Hashable
* ToJSON/FromJSON
* ToBinary/FromBinary
* SQL Types

---

# Parameters

---

```haskell
data Pair a = Pair a a
```

---

```haskell
data Pair a = Pair a a
```

```haskell
>>> :t Pair 4 5
Pair 4 5 :: Pair Int

>>> :t Pair "hi" "mom"
Pair "hi" "mom" :: Pair String
```

---

```haskell
>>> Pair 42 "gazork"

error:
    • Couldn't match type ‘String’ with ‘Int’
      Expected type: Int
        Actual type: String
    • In the second argument of ‘Pair’, namely ‘"gazork"’
      In the expression: Pair 42 "gazork"
```

---

```haskell
data Tuple a b = Tuple a b


>>> :t Tuple 42 "gazork"
Tuple 42 "gazork" :: Tuple Int String
```

---



# We Can even encode numbers!

# What problems do they solve

# How do they relate to things I know?

# Recursive types

# They're sub-sets of records

# Don't mix behaviour with data

# Let's define some

# Pattern matching & case analysis

## We're taught that booleans are the only way to branch

## In functions
## In 'case'

## vs. switch-case (says 'equality' is the only way to match)


## Breakdown into Sums and Products


# GADTs

## You can carry proofs!
## You can express relationships!
## You can enforce sequencing!



