build-lists: true
theme: Poster, 1
background-color: #0F0E0E
text: #FF5481
header: #FF5481
text-emphasis: #FFFFFF
text-strong: #FF5481
code: auto(25)


# [fit] Foldable

### *I don't have anything clever to say*

-------------------------------------------------------------------------------

## Foldable == `.toList()`

## *DONE*

-------------------------------------------------------------------------------

A simple way to represent *iteration*

-------------------------------------------------------------------------------

```haskell
class Foldable t where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    toList :: t a -> [a]
    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool
```

Only foldMap *or* foldr are required

-------------------------------------------------------------------------------

Default implementations

-------------------------------------------------------------------------------

```haskell
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool
```

-------------------------------------------------------------------------------

```haskell
instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a
```

-------------------------------------------------------------------------------

```haskell
instance Foldable Maybe where
  foldMap :: Monoid m => (a -> m) -> Maybe a -> m
  foldMap f (Just a) = f a
  foldMap _ Nothing = mempty
```

-------------------------------------------------------------------------------


```haskell
instance Foldable [] where
  foldMap :: Monoid m => (a -> m) -> [a] -> m
  foldMap f (a:rest) = f a <> foldMap f rest
  foldMap _ [] = []
```

-------------------------------------------------------------------------------

# Other Foldables

- `Map k v`
- `Set a`
- `Tuple b a`

-------------------------------------------------------------------------------

# NOT Foldable

- `(a -> b)`
- `Async a`
- `IO a`

-------------------------------------------------------------------------------
