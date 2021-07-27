{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.DriverLocation where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import Data.Swagger
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.AST as B
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Syntax as B
import qualified Database.PostgreSQL.Simple.FromField as Pg
import EulerHS.Prelude hiding (id, state)

data DriverLocationT f = DriverLocation
  { id :: B.C f (Id DriverLocation),
    lat :: B.C f (Maybe Double),
    long :: B.C f (Maybe Double),
    point :: B.C f Point,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverLocation = DriverLocationT Identity

type DriverLocationPrimaryKey = B.PrimaryKey DriverLocationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table DriverLocationT where
  data PrimaryKey DriverLocationT f = DriverLocationPrimaryKey (B.C f (Id DriverLocation))
    deriving (Generic, B.Beamable)
  primaryKey = DriverLocationPrimaryKey . id

deriving instance Show DriverLocation

deriving instance Eq DriverLocation

instance ToJSON DriverLocation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON DriverLocation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema DriverLocation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverLocationT)
fieldEMod =
  B.setEntityName "driver_location"
    <> B.modifyTableFields
      B.tableModification
        { createdAt = "created_at",
          updatedAt = "updated_at"
        }

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
