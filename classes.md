# Classes


## Semigroup

```hs
class Semigroup a where
    (<>) :: a -> a -> a
    GHC.Base.sconcat :: GHC.Base.NonEmpty a -> a
    GHC.Base.stimes :: Integral b => b -> a -> a
{-# MINIMAL (<>) #-} -- Defined in ‘GHC.Base’


instance Semigroup (Either a b)                 -- Defined in ‘Data.Either’

instance Semigroup [a]                          -- Defined in ‘GHC.Base’

instance Semigroup Ordering                     -- Defined in ‘GHC.Base’

instance Semigroup a => Semigroup (Maybe a)     -- Defined in ‘GHC.Base’
instance Semigroup a => Semigroup (IO a)        -- Defined in ‘GHC.Base’
instance Semigroup b => Semigroup (a -> b)      -- Defined in ‘GHC.Base’


instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d,
          Semigroup e) =>
         Semigroup (a, b, c, d, e)
  -- Defined in ‘GHC.Base’

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (a, b, c, d)
  -- Defined in ‘GHC.Base’

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (a, b, c)
  -- Defined in ‘GHC.Base’

instance (Semigroup a, Semigroup b) => Semigroup (a, b)
  -- Defined in ‘GHC.Base’

instance Semigroup () -- Defined in ‘GHC.Base’
```
