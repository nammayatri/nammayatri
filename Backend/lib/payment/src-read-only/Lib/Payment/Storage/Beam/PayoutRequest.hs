{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutRequest where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutRequest

data PayoutRequestT f = PayoutRequestT
  { amount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    beneficiaryId :: (B.C f Kernel.Prelude.Text),
    cashMarkedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    cashMarkedById :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    cashMarkedByName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    entityId :: (B.C f Kernel.Prelude.Text),
    entityName :: (B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName)),
    entityRefId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    expectedCreditTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    failureReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    payoutTransactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    retryCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    scheduledAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutRequestT where
  data PrimaryKey PayoutRequestT f = PayoutRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutRequestId . id

type PayoutRequest = PayoutRequestT Identity

$(enableKVPG (''PayoutRequestT) [('id)] [])

$(mkTableInstancesGenericSchema (''PayoutRequestT) "payout_request")
