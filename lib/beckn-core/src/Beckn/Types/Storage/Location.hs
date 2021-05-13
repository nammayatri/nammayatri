{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Location where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.AST as B
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Syntax as B
import qualified Database.PostgreSQL.Simple.FromField as Pg
import EulerHS.Prelude
import Servant.API

data LocationType = POINT | POLYGON | PINCODE | ADDRESS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LocationType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres LocationType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

deriving instance B.HasSqlEqualityCheck Postgres LocationType

instance ToParamSchema LocationType

instance FromHttpApiData LocationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data LocationT f = Location
  { _id :: B.C f (Id Location),
    _locationType :: B.C f LocationType,
    _lat :: B.C f (Maybe Double),
    _long :: B.C f (Maybe Double),
    _point :: B.C f Point,
    _ward :: B.C f (Maybe Text),
    _district :: B.C f (Maybe Text),
    _city :: B.C f (Maybe Text),
    _state :: B.C f (Maybe Text),
    _country :: B.C f (Maybe Text),
    _pincode :: B.C f (Maybe Text),
    _address :: B.C f (Maybe Text),
    _bound :: B.C f (Maybe Text),
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Location = LocationT Identity

type LocationPrimaryKey = B.PrimaryKey LocationT Identity

instance B.Table LocationT where
  data PrimaryKey LocationT f = LocationPrimaryKey (B.C f (Id Location))
    deriving (Generic, B.Beamable)
  primaryKey = LocationPrimaryKey . _id

deriving instance Show Location

deriving instance Eq Location

instance ToJSON Location where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Location where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Location

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LocationT)
fieldEMod =
  B.setEntityName "location"
    <> B.modifyTableFields
      B.tableModification
        { _createdAt = "created_at",
          _updatedAt = "updated_at",
          _locationType = "location_type"
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
