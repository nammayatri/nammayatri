{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Vehicle where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (..))
import EulerHS.Prelude hiding (id)
import Servant.API
import qualified Types.Storage.Organization as Org
import Utils.PostgreSQLSimple (fromFieldRead)

data Category = CAR | MOTORCYCLE | TRAIN | BUS | FLIGHT | AUTO
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Category where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres Category

instance FromBackendRow Postgres Category where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Category

instance FromHttpApiData Category where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

--------
data Variant = SEDAN | SUV | HATCHBACK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, Enum, Bounded)

instance FromField Variant where fromField = fromFieldRead "Variant"

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres Variant

instance FromBackendRow Postgres Variant where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Variant

instance FromHttpApiData Variant where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

-----
data EnergyType = PETROL | DIESEL | HYBRID | ELECTRIC | NG
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EnergyType where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres EnergyType

instance FromBackendRow Postgres EnergyType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema EnergyType

instance FromHttpApiData EnergyType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

----
data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RegistrationCategory where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RegistrationCategory where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema RegistrationCategory

instance FromHttpApiData RegistrationCategory where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data VehicleT f = Vehicle
  { id :: B.C f (Id Vehicle),
    capacity :: B.C f (Maybe Int),
    organizationId :: B.C f (Id Org.Organization),
    category :: B.C f (Maybe Category),
    make :: B.C f (Maybe Text),
    model :: B.C f (Maybe Text),
    size :: B.C f (Maybe Text),
    variant :: B.C f (Maybe Variant),
    color :: B.C f (Maybe Text),
    energyType :: B.C f (Maybe EnergyType),
    registrationNo :: B.C f Text,
    registrationCategory :: B.C f (Maybe RegistrationCategory),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Vehicle = VehicleT Identity

type VehiclePrimaryKey = B.PrimaryKey VehicleT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table VehicleT where
  data PrimaryKey VehicleT f = VehiclePrimaryKey (B.C f (Id Vehicle))
    deriving (Generic, B.Beamable)
  primaryKey = VehiclePrimaryKey . id

deriving instance Show Vehicle

deriving instance Eq Vehicle

deriving instance FromJSON Vehicle

deriving instance ToJSON Vehicle

instance ToSchema Vehicle

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity VehicleT)
fieldEMod =
  B.setEntityName "vehicle"
    <> B.modifyTableFields
      B.tableModification
        { createdAt = "created_at",
          updatedAt = "updated_at",
          energyType = "energy_type",
          registrationNo = "registration_no",
          registrationCategory = "registration_category",
          organizationId = "organization_id"
        }
