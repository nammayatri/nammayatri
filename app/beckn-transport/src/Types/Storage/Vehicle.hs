{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Vehicle where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
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
import Utils.Common
import Utils.PostgreSQLSimple (fromFieldRead)

data Category = CAR | MOTORCYCLE | TRAIN | BUS | FLIGHT | AUTO
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Category where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres Category

instance FromBackendRow Postgres Category where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse Category; invalid value: " ++ str
      Just val -> pure val

instance FromHttpApiData Category where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data Variant = SEDAN | SUV | HATCHBACK
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )

instance FromField Variant where fromField = fromFieldRead "Variant"

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres Variant

instance FromBackendRow Postgres Variant where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse Variant; invalid value: " ++ str
      Just val -> pure val

instance FromHttpApiData Variant where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

-----
data EnergyType = PETROL | DIESEL | HYBRID | ELECTRIC | NG
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EnergyType where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres EnergyType

instance FromBackendRow Postgres EnergyType where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse EnergyType; invalid value: " ++ str
      Just val -> pure val

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
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse RegistrationCategory; invalid value: " ++ str
      Just val -> pure val

instance FromHttpApiData RegistrationCategory where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data VehicleT f = Vehicle
  { id :: B.C f (Id Vehicle),
    organizationId :: B.C f (Id Org.Organization),
    variant :: B.C f Variant,
    model :: B.C f Text,
    color :: B.C f Text,
    registrationNo :: B.C f Text,
    capacity :: B.C f (Maybe Int),
    category :: B.C f (Maybe Category),
    make :: B.C f (Maybe Text),
    size :: B.C f (Maybe Text),
    energyType :: B.C f (Maybe EnergyType),
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
  primaryKey t = VehiclePrimaryKey t.id

deriving instance Show Vehicle

deriving instance Eq Vehicle

deriving instance FromJSON Vehicle

deriving instance ToJSON Vehicle

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

data VehicleAPIEntity = VehicleAPIEntity
  { id :: Id Vehicle,
    variant :: Variant,
    model :: Text,
    color :: Text,
    registrationNo :: Text,
    category :: Maybe Category,
    capacity :: Maybe Int,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

buildVehicleAPIEntity :: MonadFlow m => Vehicle -> m VehicleAPIEntity
buildVehicleAPIEntity veh = do
  return
    VehicleAPIEntity
      { id = veh.id,
        variant = veh.variant,
        model = veh.model,
        color = veh.color,
        registrationNo = veh.registrationNo,
        category = veh.category,
        capacity = veh.capacity,
        createdAt = veh.createdAt
      }
