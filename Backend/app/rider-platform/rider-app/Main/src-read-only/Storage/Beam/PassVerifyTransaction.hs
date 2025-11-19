{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PassVerifyTransaction where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PassVerifyTransactionT f = PassVerifyTransactionT
  { closingAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    destinationStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    entryGateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    exitGateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fleetId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    openingAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    purchasePassId :: B.C f Kernel.Prelude.Text,
    purchasePassPaymentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sourceStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    validTill :: B.C f Kernel.Prelude.UTCTime,
    verifiedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PassVerifyTransactionT where
  data PrimaryKey PassVerifyTransactionT f = PassVerifyTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassVerifyTransactionId . id

type PassVerifyTransaction = PassVerifyTransactionT Identity

$(enableKVPG ''PassVerifyTransactionT ['id] [['purchasePassId]])

$(mkTableInstances ''PassVerifyTransactionT "pass_verify_transaction")
