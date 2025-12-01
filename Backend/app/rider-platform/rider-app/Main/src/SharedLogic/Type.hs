module SharedLogic.Type where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

-- import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data BillingCategory = PERSONAL | BUSINESS deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BillingCategory)

$(mkHttpInstancesForEnum ''BillingCategory)

data RideType
  = NORMAL
  | RENTAL
  | INTERCITY
  | AMBULANCE
  | DELIVERY
  | METER_RIDE
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema, Eq, ToParamSchema)

$(mkHttpInstancesForEnum ''RideType)

-- | Convert RideType to BookingDetails pattern (for querying)
rideTypeToBookingDetailsPattern :: RideType -> Text
rideTypeToBookingDetailsPattern = \case
  NORMAL -> "OneWayDetails"
  RENTAL -> "RentalDetails"
  INTERCITY -> "InterCityDetails"
  AMBULANCE -> "AmbulanceDetails"
  DELIVERY -> "DeliveryDetails"
  METER_RIDE -> "MeterRideDetails"

-- | Map BillingCategory to domain type
mapBillingCategory :: BillingCategory -> Text
mapBillingCategory = \case
  BUSINESS -> "BUSINESS"
  PERSONAL -> "PERSONAL"
