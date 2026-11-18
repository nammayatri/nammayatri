{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutLifecycleEvent where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutLifecycleEvent

data PayoutLifecycleEventT f = PayoutLifecycleEventT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    batchId :: (B.C f Kernel.Prelude.Text),
    batchNo :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    driverId :: (B.C f Kernel.Prelude.Text),
    eventType :: (B.C f Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEventType),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    partnerCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    payoutOrderId :: (B.C f Kernel.Prelude.Text),
    payoutRequestId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutLifecycleEventT where
  data PrimaryKey PayoutLifecycleEventT f = PayoutLifecycleEventId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutLifecycleEventId . id

type PayoutLifecycleEvent = PayoutLifecycleEventT Identity

$(enableKVPG (''PayoutLifecycleEventT) [('id)] [[('payoutOrderId)]])

$(mkTableInstancesGenericSchema (''PayoutLifecycleEventT) "payout_lifecycle_event")
