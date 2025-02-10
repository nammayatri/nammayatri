{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonStats where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PersonStats = PersonStats
  { backlogPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    backlogPayoutStatus :: Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus,
    completedRides :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    driverCancelledRides :: Kernel.Prelude.Int,
    eveningPeakRides :: Kernel.Prelude.Int,
    isBackfilled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    morningPeakRides :: Kernel.Prelude.Int,
    offPeakRides :: Kernel.Prelude.Int,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    referralAmountPaid :: Kernel.Types.Common.HighPrecMoney,
    referralCount :: Kernel.Prelude.Int,
    referralEarnings :: Kernel.Types.Common.HighPrecMoney,
    referredByEarnings :: Kernel.Types.Common.HighPrecMoney,
    referredByEarningsPayoutStatus :: Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus,
    ticketsBookedInEvent :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    userCancelledRides :: Kernel.Prelude.Int,
    validActivations :: Kernel.Prelude.Int,
    weekdayRides :: Kernel.Prelude.Int,
    weekendPeakRides :: Kernel.Prelude.Int,
    weekendRides :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PayoutStatus = Processing | Success | Failed | ManualReview deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''PayoutStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutStatus)
