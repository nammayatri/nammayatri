{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Beckn.Storage.Esqueleto.Class where

import Beckn.Storage.Esqueleto.SqlDB (SqlDB)
import Database.Esqueleto.Experimental
import EulerHS.Prelude hiding (Key)

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntity t a
    | t -> a,
      a -> t
  where
  fromTEntity :: Entity t -> SqlDB a
  toTEntity :: a -> Entity t
  toTType :: a -> t

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntityKey t
  where
  type DomainKey t
  fromKey :: Key t -> DomainKey t
  toKey :: DomainKey t -> Key t

class QEntity a b where
  toResult :: a -> SqlDB b

instance TEntity a b => QEntity (Entity a) b where
  toResult = fromTEntity

instance ((b ~ DomainKey a), TEntityKey a) => QEntity (Value (Key a)) b where
  toResult = return . fromKey . unValue

instance QEntity (Value a) a where
  toResult = return . unValue

instance QEntity a b => QEntity (Maybe a) (Maybe b) where
  toResult a = toResult `mapM` a

instance
  ( QEntity a1 b1,
    QEntity a2 b2
  ) =>
  QEntity (a1, a2) (b1, b2)
  where
  toResult (a1, a2) =
    (,) <$> toResult a1
      <*> toResult a2

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3
  ) =>
  QEntity (a1, a2, a3) (b1, b2, b3)
  where
  toResult (a1, a2, a3) =
    (,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4
  ) =>
  QEntity (a1, a2, a3, a4) (b1, b2, b3, b4)
  where
  toResult (a1, a2, a3, a4) =
    (,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5
  ) =>
  QEntity (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
  where
  toResult (a1, a2, a3, a4, a5) =
    (,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5,
    QEntity a6 b6
  ) =>
  QEntity (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
  where
  toResult (a1, a2, a3, a4, a5, a6) =
    (,,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5
      <*> toResult a6

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5,
    QEntity a6 b6,
    QEntity a7 b7
  ) =>
  QEntity (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
  where
  toResult (a1, a2, a3, a4, a5, a6, a7) =
    (,,,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5
      <*> toResult a6
      <*> toResult a7
