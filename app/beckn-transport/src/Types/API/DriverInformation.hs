module Types.API.DriverInformation
  ( DriverInformationResponse (..),
    GetRideInfoRes (..),
    RideInfo (..),
    ListDriverRes (..),
    DriverEntityRes (..),
    LinkVehicleReq (..),
    LinkVehicleRes,
    CreateDriverReq (..),
    CreateDriverRes (..),
    validateCreateDriverReq,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Validation
import Data.Aeson
import Data.Time
import EulerHS.Prelude hiding (id)
import Types.API.Person (validatePersonReqEntity)
import qualified Types.API.Person as PersonAPI
import Types.API.Registration
import qualified Types.API.Vehicle as VehAPI
import qualified Types.Storage.Organization as Organization
import Types.Storage.Person (Person)
import Types.Storage.ProductInstance (ProductInstance)
import qualified Types.Storage.SearchReqLocation as Loc
import Types.Storage.Vehicle (Vehicle)

data DriverInformationResponse = DriverInformationResponse
  { transporter :: Organization.Organization,
    driver :: DriverEntityRes
  }
  deriving (Generic, ToJSON, FromJSON)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data RideInfo = RideInfo
  { productInstanceId :: Id ProductInstance,
    pickupLoc :: Loc.SearchReqLocation,
    dropLoc :: Loc.SearchReqLocation,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Double,
    notificationExpiryTime :: UTCTime,
    estimatedPrice :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

newtype ListDriverRes = ListDriverRes
  {drivers :: [DriverEntityRes]}
  deriving (Generic, ToJSON, FromJSON)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe Vehicle,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype LinkVehicleReq = LinkVehicleReq
  { vehicleId :: Id Vehicle
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type LinkVehicleRes = APISuccess

-- Create Person request and response
data CreateDriverReq = CreateDriverReq
  { person :: PersonAPI.PersonReqEntity,
    vehicle :: VehAPI.CreateVehicleReq
  }
  deriving (Generic, ToJSON, FromJSON)

validateCreateDriverReq :: Validate CreateDriverReq
validateCreateDriverReq CreateDriverReq {..} =
  sequenceA_
    [ validateObject "person" person validatePersonReqEntity,
      validateObject "vehicle" vehicle VehAPI.validateCreateVehicleReq
    ]

newtype CreateDriverRes = CreateDriverRes
  {driver :: UserInfoRes}
  deriving (Generic, ToJSON, FromJSON)
