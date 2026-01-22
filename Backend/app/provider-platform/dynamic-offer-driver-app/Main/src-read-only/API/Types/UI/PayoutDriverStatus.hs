{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PayoutDriverStatus where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.ScheduledPayout
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data DriverPayoutStatusEvent = DriverPayoutStatusEvent {message :: Kernel.Prelude.Maybe Data.Text.Text, status :: Domain.Types.ScheduledPayout.ScheduledPayoutStatus, timestamp :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverPayoutStatusResp = DriverPayoutStatusResp
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    expectedCreditTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Data.Text.Text,
    payoutTransactionId :: Kernel.Prelude.Maybe Data.Text.Text,
    rideId :: Data.Text.Text,
    status :: Domain.Types.ScheduledPayout.ScheduledPayoutStatus,
    statusHistory :: [DriverPayoutStatusEvent],
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
