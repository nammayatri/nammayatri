{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.TransferTransaction where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Stripe.Types.Transfer
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Extra.TransferTransaction

data TransferTransactionT f = TransferTransactionT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f Kernel.Types.Common.Currency,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    destinationAccountId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    entityId :: B.C f Kernel.Prelude.Text,
    entityName :: B.C f Lib.Payment.Domain.Types.Extra.TransferTransaction.TransferEntityName,
    errorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    idAssignedByServiceProvider :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isApiCallSuccess :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    responseDump :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    senderAccountId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Stripe.Types.Transfer.TransferStatus),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TransferTransactionT where
  data PrimaryKey TransferTransactionT f = TransferTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TransferTransactionId . id

type TransferTransaction = TransferTransactionT Identity

$(enableKVPG ''TransferTransactionT ['id] [['entityId]])

$(mkTableInstancesGenericSchema ''TransferTransactionT "transfer_transaction")
