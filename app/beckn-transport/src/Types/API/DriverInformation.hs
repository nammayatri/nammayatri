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
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.Person (Person)
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Beckn.Types.Storage.Vehicle (Vehicle)
import Beckn.Utils.Validation
import Data.Aeson
import Data.Time
import EulerHS.Prelude hiding (id)
import Types.API.Person (PersonReqEntity, validatePersonReqEntity)
import qualified Types.API.Person as PersonAPI
import Types.API.Registration
import qualified Types.API.Vehicle as VehAPI
import Types.Storage.DriverInformation

data DriverInformationResponse = DriverInformationResponse
  { transporter :: Organization.Organization,
    person :: PersonAPI.PersonEntityRes,
    driverInformation :: DriverInformation
  }
  deriving (Generic, ToJSON, FromJSON)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data RideInfo = RideInfo
  { productInstanceId :: Id ProductInstance,
    pickupLoc :: Loc.Location,
    dropLoc :: Loc.Location,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Float,
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
    active :: Bool,
    onRide :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype LinkVehicleReq = LinkVehicleReq
  { vehicleId :: Id Vehicle
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type LinkVehicleRes = APISuccess

-- Create Person request and response
data CreateDriverReq = CreateDriverReq
  { person :: PersonReqEntity,
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
