{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationFarePolicy where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Common
import qualified Tools.Beam.UtilsTH

data CancellationFarePolicy = CancellationFarePolicy
  { currency :: Kernel.Utils.Common.Currency,
    description :: Kernel.Prelude.Text,
    freeCancellationTimeSeconds :: Kernel.Types.Common.Seconds,
    id :: Kernel.Types.Id.Id Domain.Types.CancellationFarePolicy.CancellationFarePolicy,
    maxCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    maxWaitingTimeAtPickupSeconds :: Kernel.Types.Common.Seconds,
    minCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    perMetreCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    perMinuteCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    percentageOfRideFareToBeCharged :: Kernel.Types.Common.Centesimal,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
