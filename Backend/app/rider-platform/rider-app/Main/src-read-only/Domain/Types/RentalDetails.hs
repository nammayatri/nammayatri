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
  { id :: Kernel.Types.Id.Id Domain.Types.RentalDetails.RentalDetails,
    baseFare :: Kernel.Types.Common.Price,
    perHourCharge :: Kernel.Types.Common.Price,
    perExtraMinRate :: Kernel.Types.Common.Price,
    perExtraKmRate :: Kernel.Types.Common.Price,
    includedDistancePerHr :: Kernel.Types.Common.Distance,
    plannedPerKmRate :: Kernel.Types.Common.Price,
    deadKmFare :: Kernel.Types.Common.Price,
    nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Extra.RentalDetails.NightShiftInfo
  }
  deriving (Generic, Show)
