{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.OrgLocation where

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

data OrgLocationType = POINT | POLYGON | PINCODE | ADDRESS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrgLocationType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres OrgLocationType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

deriving instance B.HasSqlEqualityCheck Postgres OrgLocationType

instance ToParamSchema OrgLocationType

instance FromHttpApiData OrgLocationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data OrgLocationT f = OrgLocation
  { id :: B.C f (Id OrgLocation),
    locationType :: B.C f OrgLocationType,
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

type OrgLocation = OrgLocationT Identity

type OrgLocationPrimaryKey = B.PrimaryKey OrgLocationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table OrgLocationT where
  data PrimaryKey OrgLocationT f = OrgLocationPrimaryKey (B.C f (Id OrgLocation))
    deriving (Generic, B.Beamable)
  primaryKey = OrgLocationPrimaryKey . id

deriving instance Show OrgLocation

deriving instance Eq OrgLocation

instance ToJSON OrgLocation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON OrgLocation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema OrgLocation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OrgLocationT)
fieldEMod =
  B.setEntityName "organization_location"
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
