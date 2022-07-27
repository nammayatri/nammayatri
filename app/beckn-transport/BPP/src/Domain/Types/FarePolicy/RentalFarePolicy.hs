module Domain.Types.FarePolicy.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle as Vehicle

data RentalFarePolicy = RentalFarePolicy
  { id :: Id RentalFarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Amount,
    baseDistance :: Kilometers, -- Distance
    baseDuration :: Hours,
    extraKmFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount,
    descriptions :: [Text]
  }
  deriving (Generic, Show, Eq)

data RentalFarePolicyAPIEntity = RentalFarePolicyAPIEntity
  { id :: Id RentalFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Amount,
    baseDistance :: Kilometers, -- Distance
    baseDuration :: Hours,
    extraKmFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkDescriptions :: Amount -> Amount -> Maybe Amount -> [Text]
mkDescriptions kmRate minuteRate minuteFare =
  [ "Extra km fare: " <> amountToString kmRate,
    "Extra min fare: " <> amountToString minuteRate,
    "Extra fare for day: " <> maybe "not allowed" amountToString minuteFare,
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]

makeRentalFarePolicyAPIEntity :: RentalFarePolicy -> RentalFarePolicyAPIEntity
makeRentalFarePolicyAPIEntity RentalFarePolicy {..} = RentalFarePolicyAPIEntity {..}
