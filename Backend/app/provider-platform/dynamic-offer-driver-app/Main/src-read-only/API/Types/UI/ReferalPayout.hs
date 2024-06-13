{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ReferalPayout where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.DailyStats
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
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

data ReferralEarningsReq = ReferralEarningsReq {fromDate :: Data.Time.Calendar.Day, personId :: Kernel.Types.Id.Id Domain.Types.Person.Person, toDate :: Data.Time.Calendar.Day}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ReferralEarningsRes = ReferralEarningsRes
  { dailyEarnings :: [API.Types.UI.ReferalPayout.DailyEarning],
    orderId :: Kernel.Prelude.Maybe Data.Text.Text,
    orderStatus :: Kernel.Prelude.Maybe API.Types.UI.ReferalPayout.OrderStatus,
    referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
    totalReferralCount :: Kernel.Prelude.Int,
    vpaId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
