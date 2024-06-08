{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ReferalPayout where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time.Calendar
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
    earnings :: Kernel.Types.Common.Money,
    referrals :: Kernel.Prelude.Int,
    status :: API.Types.UI.ReferalPayout.PayoutStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PayoutStatus = Verifying | Processing | Success | Failed deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ReferalEarningRes = ReferalEarningRes {dailyEarnings :: [API.Types.UI.ReferalPayout.DailyEarning], totalReferralCount :: Kernel.Prelude.Int, vpaId :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ReferralEarningsReq = ReferralEarningsReq {fromDate :: Data.Time.Calendar.Day, personId :: Kernel.Types.Id.Id Domain.Types.Person.Person, toDate :: Data.Time.Calendar.Day}
  deriving (Generic, ToJSON, FromJSON, ToSchema)
