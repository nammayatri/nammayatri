{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.Chargeback where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Chargeback

data ChargebackT f = ChargebackT
  { id :: (B.C f Kernel.Prelude.Text),
    settlementReportId :: (B.C f Kernel.Prelude.Text),
    transactionId :: (B.C f Kernel.Prelude.Text),
    chargebackReasonCode :: (B.C f Kernel.Prelude.Text),
    chargebackAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    chargebackStatus :: (B.C f Lib.Finance.Domain.Types.Chargeback.ChargebackStatus),
    responseDeadline :: (B.C f Kernel.Prelude.UTCTime),
    evidenceUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    adminNotes :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ChargebackT where
  data PrimaryKey ChargebackT f = ChargebackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ChargebackId . id

type Chargeback = ChargebackT Identity

$(enableKVPG (''ChargebackT) [('id)] [[('settlementReportId)], [('transactionId)], [('chargebackStatus)]])

$(mkTableInstancesGenericSchema (''ChargebackT) "finance_chargeback")
