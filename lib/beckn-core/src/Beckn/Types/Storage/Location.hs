{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Location where

import Beckn.Types.Id
import Beckn.Utils.JSON
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
import EulerHS.Prelude hiding (id, state)
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
  { id :: B.C f (Id Location),
    locationType :: B.C f LocationType,
    lat :: B.C f (Maybe Double),
    long :: B.C f (Maybe Double),
    point :: B.C f Point,
    ward :: B.C f (Maybe Text),
    district :: B.C f (Maybe Text),
    city :: B.C f (Maybe Text),
    state :: B.C f (Maybe Text),
    country :: B.C f (Maybe Text),
    pincode :: B.C f (Maybe Text),
    address :: B.C f (Maybe Text),
    bound :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Location = LocationT Identity

type LocationPrimaryKey = B.PrimaryKey LocationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table LocationT where
  data PrimaryKey LocationT f = LocationPrimaryKey (B.C f (Id Location))
    deriving (Generic, B.Beamable)
  primaryKey = LocationPrimaryKey . id

deriving instance Show Location

deriving instance Eq Location

instance ToJSON Location where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Location where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Location

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LocationT)
fieldEMod =
  B.setEntityName "location"
    <> B.modifyTableFields
      B.tableModification
        { createdAt = "created_at",
          updatedAt = "updated_at",
          locationType = "location_type"
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
