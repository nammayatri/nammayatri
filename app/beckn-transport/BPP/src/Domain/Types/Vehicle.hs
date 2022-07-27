{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Vehicle where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Person as DPers
import EulerHS.Prelude hiding (id)
import Servant.API

data Category = CAR | MOTORCYCLE | TRAIN | BUS | FLIGHT | AUTO
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

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

instance FromHttpApiData Variant where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

-----
data EnergyType = PETROL | DIESEL | HYBRID | ELECTRIC | NG
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData EnergyType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

----
data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RegistrationCategory where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data Vehicle = Vehicle
  { driverId :: Id DPers.Person,
    organizationId :: Id DOrg.Organization,
    variant :: Variant,
    model :: Text,
    color :: Text,
    registrationNo :: Text,
    capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    size :: Maybe Text,
    energyType :: Maybe EnergyType,
    registrationCategory :: Maybe RegistrationCategory,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data VehicleAPIEntity = VehicleAPIEntity
  { driverId :: Id DPers.Person,
    variant :: Variant,
    model :: Text,
    color :: Text,
    registrationNo :: Text,
    category :: Maybe Category,
    capacity :: Maybe Int,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeVehicleAPIEntity :: Vehicle -> VehicleAPIEntity
makeVehicleAPIEntity Vehicle {..} = VehicleAPIEntity {..}
