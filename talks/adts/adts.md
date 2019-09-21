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

### _**A Quick note on**_

#[fit] Referential Transparency

---

#[fit] `=`
# *MEANS*
#[fit] `=`

---

Definitions:

```haskell
getX (Point x _) = x

point = Point 10 35
```

So:

[.code-highlight: 1]
[.code-highlight: 2]
[.code-highlight: 3]
[.code-highlight: all]
```haskell
  getX point
= getX (Point 10 35)    -- Def. of 'point'
= 10                    -- Def. of 'getX'
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

```haskell
moveX :: Int -> Point -> Point
moveX n (Point x y) = Point (x + n) y
```

[.code-highlight: 1]
[.code-highlight: 2]
[.code-highlight: 3]
[.code-highlight: 4]
[.code-highlight: all]
```haskell
  moveX 5 point
= moveX 5 (Point 10 35) -- Def. of 'point'
= Point (10 + 5) 35     -- Def. of 'moveX'
= Point 15 35           -- Evaluate addition
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

#[fit] Bools
### *are an*
#[fit] enum

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

# We can even partially apply it and DELAY the 'if'

```haskell
choose :: a -> a -> Bool -> a
choose x _ True = x
choose _ y False = y
```

[.code-highlight: 0]
[.code-highlight: 1-2]
[.code-highlight: 3]
[.code-highlight: 3-4]
[.code-highlight: all]
```haskell
>>> let check :: Int -> String
        check = choose "HI" "LOW" . (>10)
>>> map check [3, 15, 6, 17, 13]
["HI","LOW","HI","LOW","LOW"]
```

---

#[fit] Haskell
## *doesn't need*
#[fit] **if**

---

Where were we again?

---

Oh right...

---

#[fit] Case Statements

### Bools aren't the only way to branch

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

```haskell
data ContactMethod = 
    Email String
  | Address String
  | Phone String
```

---

```haskell
contactUser :: String -> ContactMethod -> IO ()
contactUser msg method = 
  case method of
    Email email      -> sendEmail msg email
    Address address  -> sendLetter msg email
    Phone phNumber   -> sendText msg email
```

---

```python
class Email:
  def __init__(self, email):
      self.email = email

  def contact(self, msg):
    self.sendEmail(msg)
```

---

```python
class Email:
  def __init__(self, email):
      self.email = email

  def contact(self, msg):
    self.sendEmail(msg)

class Address:
  def __init__(self, email):
      self.email = email

  def contact(self, msg):
    self.sendLetter(msg)

class Phone:
  def __init__(self, email):
      self.email = email

  def contact(self, msg):
    self.sendText(msg)
```

---

#[fit] ADTs 
## are
#[fit] *CLOSED*

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

[.code-highlight: 1-3]
[.code-highlight: all]
```haskell
distance :: Point' -> Point' -> Double
distance (Point' {x = x1, y = y1})
         (Point' {x = x2, y = y2}) =
    sqrt(fromIntegral ( (x2 - x1)^2 
                      + (y2 - y1)^2))
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

# Derived Ordering

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
```

---

```haskell
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

* `Hashable`
* `ToJSON/FromJSON`
* `ToBinary/FromBinary`
* `SQL Types`

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

[.code-highlight: 1-2]
[.code-highlight: 4-5]
[.code-highlight: all]
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

```haskell
data Either a b =
    Left a
  | Right b


>>> :t Left 42
Left 42 :: Either Int b
>>> :t Right "hello"
Right "hello" :: Either a String
```

---

```haskell
sizeOf :: Either String Int
sizeOf (Left s) = length s
sizeOf (Right n) = n
```

---

#[fit] Recursive types

--- 

# Lists

```haskell
data List a =  Nil | Node a (List a)
```

---

```haskell
data List a =  Nil | Node a (List a)

data [a] = [] | a : [a]
```

---

# Trees

```haskell
data BinTree a =  
    Leaf 
  | Node (BinTree a) a (BinTree a)
```

---

# Nullary Types

---

# [fit] `data Void`

---

```haskell
data Void

Type Parser e a = String -> Either e a

alwaysSucceeds :: Parser Void Int
alwaysFails :: Parser Error Void 
```

---

```haskell
server :: Config -> IO Void
server config = do
  serveRequest
  server config
```

---

# They can help make code more correct

```haskell
data Bound
data Unbound

data Socket isBound = Socket Int

bind    :: Socket Unbound -> IO (Socket Bound)
connect :: Socket Bound   -> IO ()
unbind  :: Socket Bound   -> IO ()
```
