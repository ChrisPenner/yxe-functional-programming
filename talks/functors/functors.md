build-lists: true
theme: Poster, 1
background-color: #0F0E0E
text: #FF5481
header: #FF5481
text-emphasis: #FFFFFF
text-strong: #FF5481
code: auto(25)


# [fit] Functors

### *map the world*

-------------------------------------------------------------------------------

## Functors == `.map()`

## *DONE*

-------------------------------------------------------------------------------

# Primer on Higher Kinded Types

-------------------------------------------------------------------------------

# *Higher* Kinded Types

-------------------------------------------------------------------------------

# Higher *Kinded* Types

-------------------------------------------------------------------------------

# Kinds

### are the  __Types__ of __Types__

-------------------------------------------------------------------------------

- `[a] :: Type`
- `[] :: a -> Type`
- `[] :: Type -> Type`

-------------------------------------------------------------------------------

- `Map k v :: Type`
- `Map k :: Type -> Type`
- `Map :: Type -> Type -> Type`

-------------------------------------------------------------------------------

- `IO String :: Type`
- `IO :: Type -> Type`

-------------------------------------------------------------------------------

# Higher Kinded *Types*

-------------------------------------------------------------------------------

`map :: (a -> b) -> [a] -> [b]`

-------------------------------------------------------------------------------

```haskell
map  ::              (a -> b) -> [a] -> [b]
fmap :: Functor f => (a -> b) -> f a -> f b
```

-------------------------------------------------------------------------------

```haskell
class Functor (f :: Type -> Type) where
    fmap :: (a -> b) -> f a -> f b
```

-------------------------------------------------------------------------------

```haskell
newtype Identity a = Identity a

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)
```

-------------------------------------------------------------------------------

```haskell
newtype Tuple first second = Tuple first second

instance Functor (Tuple first) where
  fmap :: (a -> b) -> Tuple first a -> Tuple first b
  fmap f (Tuple first second) = Tuple first (f second)
```

-------------------------------------------------------------------------------

```haskell
newtype Const first second = Const first

instance Functor (Const first) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap f (Const c) = Const c
```

-------------------------------------------------------------------------------

```haskell
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap f (a:rest) = f a : fmap f rest
    fmap _ [] = []
```

-------------------------------------------------------------------------------

# Other Functors

- `Map k v`
- `IO a`
- `Maybe a`
- `Either l r`
- `Async a`
- `a -> b`

---

# NOT (Covariant) Functors

```haskell
newtype Predicate a = 
    Predicate (a -> Bool)
```

-------------------------------------------------------------------------------

- `Set a`
