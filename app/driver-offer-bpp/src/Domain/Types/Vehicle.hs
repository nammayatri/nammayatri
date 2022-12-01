{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Vehicle (module Domain.Types.Vehicle, module Reexport) where

import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPers
import Domain.Types.Vehicle.Variant as Reexport
import EulerHS.Prelude hiding (id)
import Servant.API

data Category = CAR | MOTORCYCLE | TRAIN | BUS | FLIGHT | AUTO_CATEGORY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable Category

instance FromHttpApiData Category where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

----
data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable RegistrationCategory

instance FromHttpApiData RegistrationCategory where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data Vehicle = Vehicle
  { driverId :: Id DPers.Person,
    merchantId :: Id DM.Merchant,
    variant :: Reexport.Variant,
    model :: Text,
    color :: Text,
    registrationNo :: Text,
    capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    size :: Maybe Text,
    energyType :: Maybe Text,
    registrationCategory :: Maybe RegistrationCategory,
    vehicleClass :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, PrettyShow)

data VehicleAPIEntity = VehicleAPIEntity
  { driverId :: Id DPers.Person,
    variant :: Reexport.Variant,
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
