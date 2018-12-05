build-lists: true
theme: Poster, 1
background-color: #0F0E0E
text: #FF5481
header: #FF5481
text-emphasis: #FFFFFF
text-strong: #FF5481
code: auto(25)


# [fit] Data Types

![inline](./data-typing.jpg)

-------------------------------------------------------------------------------

```haskell
data [] a = [] | a : [a]
```

-------------------------------------------------------------------------------

```haskell
data Maybe = Just a | Nothing
```

-------------------------------------------------------------------------------

```haskell
data Either l r = Left l | Right r
```

-------------------------------------------------------------------------------

```haskell
data Const a b = Const a
```

-------------------------------------------------------------------------------

```haskell
data Tagged t a = Tagged a
```

-------------------------------------------------------------------------------

```haskell
data IO a = ???
```

-------------------------------------------------------------------------------

```haskell
data () = ()
```

-------------------------------------------------------------------------------

```haskell
data Void
```

-------------------------------------------------------------------------------

```haskell
data Car = Car 
    { make :: String
    , model :: String
    , year :: Int
    }
```

-------------------------------------------------------------------------------


```haskell
data Car a = Car 
    { make :: String
    , model :: String
    , year :: Int
    , contents :: a
    }

make :: Car a -> String
contents :: Car a -> a
```

-------------------------------------------------------------------------------

```haskell
data    Email = Email String
-- vs
newtype Email = Email String
-- vs
type    Email = String
```
