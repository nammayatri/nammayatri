{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.InterCityDetails (module Domain.Types.InterCityDetails, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.InterCityDetails as ReExport
import qualified Domain.Types.Extra.RentalDetails
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data InterCityDetails = InterCityDetails
  { baseFare :: Kernel.Types.Common.Price,
    deadKmFare :: Kernel.Types.Common.Price,
    id :: Kernel.Types.Id.Id Domain.Types.InterCityDetails.InterCityDetails,
    kmPerPlannedExtraHour :: Kernel.Types.Common.Distance,
    nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Extra.RentalDetails.NightShiftInfo,
    perDayMaxAllowanceInMins :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    perDayMaxHourAllowance :: Kernel.Types.Common.Hours,
    perExtraKmRate :: Kernel.Types.Common.Price,
    perExtraMinRate :: Kernel.Types.Common.Price,
    perHourCharge :: Kernel.Types.Common.Price,
    plannedPerKmRateOneWay :: Kernel.Types.Common.Price,
    plannedPerKmRateRoundTrip :: Kernel.Types.Common.Price,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
