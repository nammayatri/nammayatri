{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PersonDailyOfferStats where

import qualified Data.Time.Calendar
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data PersonDailyOfferStats = PersonDailyOfferStats
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    date :: Data.Time.Calendar.Day,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    offerCount :: Kernel.Prelude.Int,
    payoutStatus :: Lib.Payment.Domain.Types.Common.PayoutStatus,
    personId :: Kernel.Prelude.Text,
    totalCashbackAmount :: Kernel.Types.Common.HighPrecMoney,
    totalDiscountAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (ToJSON), (FromJSON))
