{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutStatusHistory where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id

data PayoutStatusHistory = PayoutStatusHistory
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    scheduledPayoutId :: Kernel.Prelude.Text,
    status :: Lib.Payment.Domain.Types.PayoutStatusHistory.ScheduledPayoutStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data ScheduledPayoutStatus
  = INITIATED
  | PROCESSING
  | CREDITED
  | AUTO_PAY_FAILED
  | RETRYING
  | FAILED
  | CANCELLED
  | CASH_PAID
  | CASH_PENDING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ScheduledPayoutStatus))
