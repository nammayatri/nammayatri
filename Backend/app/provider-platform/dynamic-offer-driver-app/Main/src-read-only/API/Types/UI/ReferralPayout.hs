{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ReferralPayout where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.DailyStats
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Juspay.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data DailyEarning = DailyEarning
  { activatedItems :: Kernel.Prelude.Int,
    earningDate :: Data.Time.Calendar.Day,
    earnings :: Kernel.Types.Common.HighPrecMoney,
    payoutOrderId :: Kernel.Prelude.Maybe Data.Text.Text,
    referrals :: Kernel.Prelude.Int,
    status :: Domain.Types.DailyStats.PayoutStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReferralEarningsRes = ReferralEarningsRes
  { dailyEarnings :: [DailyEarning],
    orderId :: Kernel.Prelude.Maybe Data.Text.Text,
    orderStatus :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.Common.TransactionStatus,
    payoutRegistrationAmount :: Kernel.Types.Common.HighPrecMoney,
    referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
    totalReferralCount :: Kernel.Prelude.Int,
    vpaId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
