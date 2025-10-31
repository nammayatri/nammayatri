{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.Refunds where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common

data RefundsT f = RefundsT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    errorCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    errorMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    idAssignedByServiceProvider :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    initiatedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    isApiCallSuccess :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    orderId :: (B.C f Kernel.Prelude.Text),
    refundAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    shortId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Kernel.External.Payment.Interface.RefundStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RefundsT where
  data PrimaryKey RefundsT f = RefundsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RefundsId . id

type Refunds = RefundsT Identity

$(enableKVPG (''RefundsT) [('id)] [[('orderId)]])

$(mkTableInstancesGenericSchema (''RefundsT) "refunds")
