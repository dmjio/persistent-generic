persistent-generic
==========================

Generic facilities for working with `Persistent` classes.

```haskell
{-# language DeriveGeneric #-}

module Main where

import Database.Persist.Generic

data StopLight = Red | Yellow | Green
  deriving (Show, Eq, Generic)

instance PersistField StopLight where
  toPersistValue = genericToPersistValue
  fromPersistValue = genericFromPersistValue
```

### Limitations

  - Only `PersistField` is currently supported.