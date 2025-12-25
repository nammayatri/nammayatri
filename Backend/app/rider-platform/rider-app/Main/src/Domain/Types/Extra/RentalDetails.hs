{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.RentalDetails where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.JSON (removeNullFields)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Price,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show)

data NightShiftInfoAPIEntity = NightShiftInfoAPIEntity
  { nightShiftCharge :: Money,
    nightShiftChargeWithCurrency :: PriceAPIEntity,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RentalDetailsAPIEntity = RentalDetailsAPIEntity
  { baseFare :: Money,
    perHourCharge :: Money,
    perExtraMinRate :: Money,
    baseFareWithCurrency :: PriceAPIEntity,
    perHourChargeWithCurrency :: PriceAPIEntity,
    perExtraMinRateWithCurrency :: PriceAPIEntity,
    includedKmPerHr :: Kilometers,
    includedDistancePerHrWithUnit :: Distance,
    plannedPerKmRate :: Money,
    perExtraKmRate :: Money,
    plannedPerKmRateWithCurrency :: PriceAPIEntity,
    perExtraKmRateWithCurrency :: PriceAPIEntity,
    nightShiftInfo :: Maybe NightShiftInfoAPIEntity,
    tollCharges :: Maybe PriceAPIEntity,
    deadKmFare :: PriceAPIEntity
  }
  deriving (Generic, FromJSON, Show, ToSchema)

instance ToJSON RentalDetailsAPIEntity where
  toJSON = genericToJSON removeNullFields

mkNightShiftInfoAPIEntity :: NightShiftInfo -> NightShiftInfoAPIEntity
mkNightShiftInfoAPIEntity NightShiftInfo {..} =
  NightShiftInfoAPIEntity
    { nightShiftCharge = nightShiftCharge.amountInt,
      nightShiftChargeWithCurrency = mkPriceAPIEntity nightShiftCharge,
      ..
    }
