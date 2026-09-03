{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CustomerBlockTransactions where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.CustomerBlockTransactions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CustomerBlockTransactionsT f = CustomerBlockTransactionsT
  { actionType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.CustomerBlockTransactions.ActionType)),
    blockLiftTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    blockReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    blockTimeInHours :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    blockedBy :: (B.C f Domain.Types.CustomerBlockTransactions.BlockedBy),
    customerId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    reasonCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reportedAt :: (B.C f Kernel.Prelude.UTCTime),
    requestorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table CustomerBlockTransactionsT where
  data PrimaryKey CustomerBlockTransactionsT f = CustomerBlockTransactionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CustomerBlockTransactionsId . id

type CustomerBlockTransactions = CustomerBlockTransactionsT Identity

$(enableKVPG (''CustomerBlockTransactionsT) [('id)] [[('customerId)]])

$(mkTableInstances (''CustomerBlockTransactionsT) "customer_block_transactions")
