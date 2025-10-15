{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SubscriptionTransaction where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SubscriptionTransaction
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Juspay.Types.Common
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SubscriptionTransactionT f = SubscriptionTransactionT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Data.Time.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    entityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    runningBalance :: B.C f Kernel.Types.Common.HighPrecMoney,
    status :: B.C f Kernel.External.Payment.Juspay.Types.Common.TransactionStatus,
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    transactionType :: B.C f Domain.Types.SubscriptionTransaction.TransactionType,
    updatedAt :: B.C f Data.Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SubscriptionTransactionT where
  data PrimaryKey SubscriptionTransactionT f = SubscriptionTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SubscriptionTransactionId . id

type SubscriptionTransaction = SubscriptionTransactionT Identity

$(enableKVPG ''SubscriptionTransactionT ['id] [])

$(mkTableInstances ''SubscriptionTransactionT "subscription_transaction")
