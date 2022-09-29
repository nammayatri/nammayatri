module Domain.Types.FarePolicy.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle as Vehicle

data RentalFarePolicy = RentalFarePolicy
  { id :: Id RentalFarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Money,
    baseDistance :: Kilometers, -- Distance
    baseDuration :: Hours,
    extraKmFare :: HighPrecMoney,
    extraMinuteFare :: HighPrecMoney,
    driverAllowanceForDay :: Maybe Money,
    descriptions :: [Text]
  }
  deriving (Generic, Show, Eq)

data RentalFarePolicyAPIEntity = RentalFarePolicyAPIEntity
  { id :: Id RentalFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Money,
    baseDistance :: Kilometers, -- Distance
    baseDuration :: Hours,
    extraKmFare :: HighPrecMoney,
    extraMinuteFare :: HighPrecMoney,
    driverAllowanceForDay :: Maybe Money
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkDescriptions :: HighPrecMoney -> HighPrecMoney -> Maybe Money -> [Text]
mkDescriptions kmRate minuteRate minuteFare =
  [ "Extra km fare: " <> show kmRate,
    "Extra min fare: " <> show minuteRate,
    "Extra fare for day: " <> maybe "not allowed" show minuteFare,
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]

makeRentalFarePolicyAPIEntity :: RentalFarePolicy -> RentalFarePolicyAPIEntity
makeRentalFarePolicyAPIEntity RentalFarePolicy {..} = RentalFarePolicyAPIEntity {..}
