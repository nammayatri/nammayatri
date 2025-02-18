{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.InterCityDetails where

import Data.Aeson
import Domain.Types.RentalDetails
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.JSON (removeNullFields)

-- Extra code goes here --
data InterCityDetailsAPIEntity = InterCityDetailsAPIEntity
  { quoteId :: Text,
    baseFare :: PriceAPIEntity,
    perHourCharge :: PriceAPIEntity,
    perExtraMinRate :: PriceAPIEntity,
    perExtraKmRate :: PriceAPIEntity,
    deadKmFare :: PriceAPIEntity,
    plannedPerKmRateOneWay :: PriceAPIEntity,
    plannedPerKmRateRoundTrip :: PriceAPIEntity,
    kmPerPlannedExtraHour :: Distance,
    nightShiftInfo :: Maybe NightShiftInfoAPIEntity,
    perDayMaxHourAllowance :: Hours,
    perDayMaxAllowanceInMins :: Maybe Minutes,
    tollCharges :: Maybe PriceAPIEntity,
    roundTrip :: Maybe Bool
  }
  deriving (Generic, FromJSON, Show, ToSchema)

instance ToJSON InterCityDetailsAPIEntity where
  toJSON = genericToJSON removeNullFields
