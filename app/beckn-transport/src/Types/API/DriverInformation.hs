module Types.API.DriverInformation
  ( ActiveDriversResponse (..),
    DriverRidesInformation (..),
    DriverInformationResponse (..),
    GetRideInfoRes (..),
    RideInfo (..),
  )
where

import Beckn.Types.App (PersonId, ProductInstanceId)
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Organization as Organization
import Data.Time
import EulerHS.Prelude
import qualified Types.API.Person as PersonAPI
import Types.Storage.DriverInformation

newtype ActiveDriversResponse = ActiveDriversResponse
  { active_drivers :: [DriverRidesInformation]
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverRidesInformation = DriverRidesInformation
  { driver_id :: PersonId,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverInformationResponse = DriverInformationResponse
  { transporter :: Organization.Organization,
    person :: PersonAPI.PersonEntityRes,
    driverInformation :: DriverInformation
  }
  deriving (Generic, ToJSON)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON)

data RideInfo = RideInfo
  { productInstanceId :: ProductInstanceId,
    pickupLoc :: Loc.Location,
    dropLoc :: Loc.Location,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Float,
    notificationExpiryTime :: UTCTime,
    estimatedPrice :: Text
  }
  deriving (Generic, ToJSON)
