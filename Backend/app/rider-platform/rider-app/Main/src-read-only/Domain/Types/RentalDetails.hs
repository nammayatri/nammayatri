{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RentalDetails (module Domain.Types.RentalDetails, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.RentalDetails as ReExport
import qualified Domain.Types.Extra.RentalDetails
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RentalDetails = RentalDetails
  { baseFare :: Kernel.Types.Common.Price,
    deadKmFare :: Kernel.Types.Common.Price,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    id :: Kernel.Types.Id.Id Domain.Types.RentalDetails.RentalDetails,
    includedKmPerHr :: Kernel.Types.Common.Kilometers,
    nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Extra.RentalDetails.NightShiftInfo,
    perExtraKmRate :: Kernel.Types.Common.Price,
    perExtraMinRate :: Kernel.Types.Common.Price,
    perHourCharge :: Kernel.Types.Common.Price,
    plannedPerKmRate :: Kernel.Types.Common.Price
  }
  deriving (Generic, Show)
