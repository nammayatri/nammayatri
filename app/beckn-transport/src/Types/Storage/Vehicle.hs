{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Vehicle where

import           Types.App
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BSL
import           Data.Swagger
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as DT
import           Data.Time.LocalTime
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude
import           Servant.API
import           Servant.Swagger

data Category = CAR | MOTORCYCLE | TRAIN | BUS | FLIGHT | AUTO
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Category where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Category where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Category
instance FromHttpApiData Category where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

-------- 
data Variant = SEDAN | SUV | COMPACT | PASSENGER | METRO | AIRBUS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Variant where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Variant
instance FromHttpApiData Variant where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict
-----
data EnergyType = PETROL | DIESEL | HYBRID | ELECTRIC | NG
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EnergyType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL EnergyType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema EnergyType
instance FromHttpApiData EnergyType where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

----
data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RegistrationCategory where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL RegistrationCategory where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema RegistrationCategory
instance FromHttpApiData RegistrationCategory where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict


data VehicleT f =
  Vehicle
    { _id               :: B.C f VehicleId
    , _capacity         :: B.C f (Maybe Int)
    , _category         :: B.C f (Maybe Category)
    , _make             :: B.C f (Maybe Text)
    , _model            :: B.C f (Maybe Text)
    , _size             :: B.C f (Maybe Text)
    , _variant          :: B.C f (Maybe Variant)
    , _color            :: B.C f (Maybe Text)
    , _energyType       :: B.C f (Maybe EnergyType)
    , _registrationNo   :: B.C f Text
    , _registrationCategory     :: B.C f (Maybe RegistrationCategory)
    , _createdAt        :: B.C f LocalTime
    , _updatedAt        :: B.C f LocalTime
    }

  deriving (Generic, B.Beamable)

type Vehicle = VehicleT Identity

type VehiclePrimaryKey = B.PrimaryKey VehicleT Identity

instance B.Table VehicleT where
  data PrimaryKey VehicleT f = VehiclePrimaryKey (B.C f VehicleId)
                               deriving (Generic, B.Beamable)
  primaryKey = VehiclePrimaryKey . _id

deriving instance Show Vehicle

deriving instance Eq Vehicle

instance ToJSON Vehicle where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Vehicle where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Vehicle

insertExpression org = insertExpressions [org]

insertExpressions orgs = B.insertValues orgs


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity VehicleT)
fieldEMod =
  B.setEntityName "booking_reference" <>
    B.modifyTableFields
      B.tableModification
        { _createdAt = "created_at"
        , _updatedAt = "updated_at"
        , _energyType = "energy_type"
        , _registrationNo = "registration_no"
        , _registrationCategory = "registration_category"
        }
