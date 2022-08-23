module Domain.Types.Ride.API where

import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Aeson
import qualified Domain.Types.Person as DPers
import qualified Domain.Types.Rating as DRating
import Domain.Types.Ride.Type
import qualified Domain.Types.Vehicle as DVeh
import Beckn.Prelude

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    computedTotalFare :: Maybe Money,
    actualRideDistance :: Meters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> DPers.DecryptedPerson -> DVeh.Vehicle -> Maybe DRating.Rating -> RideAPIEntity
makeRideAPIEntity ride driver vehicle mbRating =
  RideAPIEntity
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      driverName = driver.firstName,
      driverNumber = driver.mobileCountryCode <> driver.mobileNumber,
      vehicleNumber = vehicle.registrationNo,
      vehicleColor = vehicle.color,
      vehicleVariant = vehicle.variant,
      vehicleModel = vehicle.model,
      computedFare = ride.fare,
      computedTotalFare = ride.totalFare,
      actualRideDistance = roundToIntegral ride.traveledDistance,
      rideRating = mbRating <&> (.ratingValue),
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt
    }

