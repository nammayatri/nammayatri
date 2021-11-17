{-# LANGUAGE FunctionalDependencies #-}

module Beckn.Storage.Esqueleto.Types where

import Beckn.Types.Id (Id)
import Data.OpenApi (ToSchema)
import Database.Beam.Backend
import qualified Database.Beam.Backend.SQL.AST as B
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Postgres.Syntax as B
import Database.Persist.Postgresql
import qualified Database.PostgreSQL.Simple.FromField as Pg
import EulerHS.Prelude hiding (Key)

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntity t a
    | t -> a,
      a -> t
  where
  fromTEntity :: MonadThrow m => Entity t -> m a
  toTEntity :: a -> Entity t
  toTType :: a -> t

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntityKey t a
    | t -> a,
      a -> t
  where
  fromKey :: Key t -> Id a
  toKey :: a -> Key t

data Point = Point
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, ToSchema)

instance HasSqlValueSyntax B.Value Point where
  sqlValueSyntax _ = sqlValueSyntax SqlNull

instance HasSqlValueSyntax B.PgValueSyntax Point where
  sqlValueSyntax _ = sqlValueSyntax SqlNull

instance FromBackendRow Postgres Point

instance Pg.FromField Point where
  fromField _ Nothing = return Point
  fromField _ (Just _) = return Point

instance PersistField Point where
  toPersistValue _ = PersistNull
  fromPersistValue _ = return Point

instance PersistFieldSql Point where
  sqlType _ = SqlOther "geography"
