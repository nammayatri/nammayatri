module Domain.Types.FarePolicy.RestrictedExtraFare where

import Domain.Types.Merchant
import qualified Domain.Types.Vehicle.Variant as Vehicle
import Kernel.Prelude
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Id

data RestrictedExtraFare = RestrictedExtraFare
  { id :: Id RestrictedExtraFare,
    merchantId :: Id Merchant,
    vehicleVariant :: Vehicle.Variant,
    minTripDistance :: Meters,
    driverMaxExtraFare :: Money
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)