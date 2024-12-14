{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Database.Persist.Generic
-- Copyright   : (c) 2025 David Johnson
-- License     : All Rights Reserved
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Generic facilities for dealing with Persistent classes.
--
--------------------------------------------------------------------------------
module Database.Persist.Generic
  ( -- * Classes
    GToPersistValue (..)
  , GFromPersistValue (..)
    -- * Methods
  , genericToPersistValue
  , genericFromPersistValue
  ) where

import           Control.Applicative  ((<|>))
import           Data.Bifunctor       (first)
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Persist.Sql (PersistValue (PersistText), toPersistValue, fromPersistValue)
import           GHC.Generics         (Generic (..), D1, C1, M1(..), (:+:)(..), Meta(..), U1(..))
import           GHC.TypeLits         (KnownSymbol, symbolVal)

-- | Generic deriving of 'toPersistValue'
genericToPersistValue :: (Generic a, GToPersistValue (Rep a)) => a -> PersistValue
genericToPersistValue = gToPersistValue . from

-- | Generic deriving of 'fromPersistValue'
genericFromPersistValue
  :: (Generic a, GFromPersistValue (Rep a))
  => PersistValue
  -> Either Text a
genericFromPersistValue v =
  first T.pack (to <$> gFromPersistValue v)

-- | Generic class for deriving 'PersistValue'
class GToPersistValue f where
  gToPersistValue :: f a -> PersistValue

instance GToPersistValue a => GToPersistValue (D1 f a) where
  gToPersistValue (M1 x) = gToPersistValue x

instance KnownSymbol name => GToPersistValue (C1 ('MetaCons name x y) U1) where
  gToPersistValue (M1 _) = PersistText (T.pack name)
    where
      name = symbolVal (Proxy @ name)

instance (GToPersistValue l, GToPersistValue r) => GToPersistValue (l :+: r) where
  gToPersistValue (L1 x) = gToPersistValue x
  gToPersistValue (R1 x) = gToPersistValue x

-- | Generic class for parsing 'PersistValue'
class GFromPersistValue f where
  gFromPersistValue :: PersistValue -> Either String (f a)

instance GFromPersistValue a => GFromPersistValue (D1 f a) where
  gFromPersistValue x = M1 <$> gFromPersistValue x

instance KnownSymbol name => GFromPersistValue (C1 ('MetaCons name x y) U1) where
  gFromPersistValue (PersistText v) =
    if T.unpack v == name
      then pure (M1 U1)
      else Left $ "Parse error: " <> name
    where
      name = symbolVal (Proxy @ name)
  gFromPersistValue _ = Left $ "Invalid Type: " <> name
    where
      name = symbolVal (Proxy @ name)

instance (GFromPersistValue l, GFromPersistValue r) => GFromPersistValue (l :+: r) where
  gFromPersistValue x = l <|> r
    where
      l = L1 <$> gFromPersistValue x
      r = R1 <$> gFromPersistValue x
