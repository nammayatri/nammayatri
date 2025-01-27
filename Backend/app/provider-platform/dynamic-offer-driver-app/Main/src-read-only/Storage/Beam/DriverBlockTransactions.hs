{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverBlockTransactions where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverBlockTransactions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH
import qualified Tools.Error

data DriverBlockTransactionsT f = DriverBlockTransactionsT
  { actionType :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverBlockTransactions.ActionType),
    blockLiftTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    blockReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockReasonFlag :: B.C f (Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag),
    blockTimeInHours :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    blockedBy :: B.C f Domain.Types.DriverBlockTransactions.BlockedBy,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    reasonCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    reportedAt :: B.C f Kernel.Prelude.UTCTime,
    requestorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverBlockTransactionsT where
  data PrimaryKey DriverBlockTransactionsT f = DriverBlockTransactionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverBlockTransactionsId . id

type DriverBlockTransactions = DriverBlockTransactionsT Identity

$(enableKVPG ''DriverBlockTransactionsT ['id] [['driverId]])

$(mkTableInstances ''DriverBlockTransactionsT "driver_block_transactions")
