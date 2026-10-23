{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.Transfer where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Transfer
import Tools.Beam.UtilsTH

data TransferT f = TransferT
  { accountId :: (B.C f Kernel.Prelude.Text),
    amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    attemptCount :: (B.C f Kernel.Prelude.Int),
    counterAccountId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    coveredEntryIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    direction :: (B.C f Lib.Finance.Domain.Types.Transfer.TransferDirection),
    entryIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    feeAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    holdEntryId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    id :: (B.C f Kernel.Prelude.Text),
    lastError :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    railKey :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    railOrderId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    railStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    referenceId :: (B.C f Kernel.Prelude.Text),
    referenceType :: (B.C f Kernel.Prelude.Text),
    requestedAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    scheduledFor :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Lib.Finance.Domain.Types.Transfer.TransferStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TransferT where
  data PrimaryKey TransferT f = TransferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TransferId . id

type Transfer = TransferT Identity

$(enableKVPG (''TransferT) [('id)] [[('accountId)], [('railOrderId)], [('referenceId)]])

$(mkTableInstancesGenericSchema (''TransferT) "finance_transfer")
