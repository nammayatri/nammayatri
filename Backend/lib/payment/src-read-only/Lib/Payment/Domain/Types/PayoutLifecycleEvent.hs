{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutLifecycleEvent where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Payment.Domain.Types.PayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Tools.Beam.UtilsTH

data PayoutLifecycleEvent = PayoutLifecycleEvent
  { amount :: Kernel.Types.Common.HighPrecMoney,
    batchId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch,
    batchNo :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    driverId :: Kernel.Prelude.Text,
    eventType :: Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEventType,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    partnerCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder,
    payoutRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data PayoutLifecycleEventType
  = INITIATED
  | SETTLED_AT_BANK
  | FAILED
  | REVERSED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutLifecycleEventType)

$(mkHttpInstancesForEnum ''PayoutLifecycleEventType)
