{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ReferralPayout where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.DailyStats
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data DailyEarning = DailyEarning
  { activatedItems :: Kernel.Prelude.Int,
    earningDate :: Data.Time.Calendar.Day,
    earnings :: Kernel.Types.Common.HighPrecMoney,
    payoutOrderId :: Kernel.Prelude.Maybe Data.Text.Text,
    payoutOrderStatus :: Kernel.Prelude.Maybe Data.Text.Text,
    referrals :: Kernel.Prelude.Int,
    status :: Domain.Types.DailyStats.PayoutStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OrderStatus = SuccessFul | Failed | Pending deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ReferralEarningsRes = ReferralEarningsRes
  { dailyEarnings :: [API.Types.UI.ReferralPayout.DailyEarning],
    orderId :: Kernel.Prelude.Maybe Data.Text.Text,
    orderStatus :: Kernel.Prelude.Maybe API.Types.UI.ReferralPayout.OrderStatus,
    referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
    totalReferralCount :: Kernel.Prelude.Int,
    vpaId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
