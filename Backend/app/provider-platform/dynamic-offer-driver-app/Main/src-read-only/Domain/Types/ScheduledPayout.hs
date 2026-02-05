{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ScheduledPayout where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PayoutStatusHistory
import qualified Tools.Beam.UtilsTH

data ScheduledPayout = ScheduledPayout
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bookingId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Text,
    expectedCreditTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout,
    markCashPaidBy :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    payoutTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideId :: Kernel.Prelude.Text,
    status :: Lib.Payment.Domain.Types.PayoutStatusHistory.ScheduledPayoutStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
