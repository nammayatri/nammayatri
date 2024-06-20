{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
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
  { id :: Kernel.Types.Id.Id Domain.Types.InterCityDetails.InterCityDetails,
    baseFare :: Kernel.Types.Common.Price,
    perHourCharge :: Kernel.Types.Common.Price,
    perExtraMinRate :: Kernel.Types.Common.Price,
    perExtraKmRate :: Kernel.Types.Common.Price,
    deadKmFare :: Kernel.Types.Common.Price,
    kmPerPlannedExtraHour :: Kernel.Types.Common.Distance,
    plannedPerKmRateOneWay :: Kernel.Types.Common.Price,
    plannedPerKmRateRoundTrip :: Kernel.Types.Common.Price,
    perDayMaxHourAllowance :: Kernel.Types.Common.Hours,
    nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Extra.RentalDetails.NightShiftInfo,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
