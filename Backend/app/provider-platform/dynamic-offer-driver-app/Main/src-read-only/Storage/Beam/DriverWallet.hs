{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverWallet where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverWallet
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverWalletT f = DriverWalletT
  { collectionAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    createdAt :: B.C f Data.Time.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    driverPayable :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    gstDeduction :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    merchantPayable :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    payoutOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverWallet.PayoutStatus),
    rideId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    runningBalance :: B.C f Kernel.Types.Common.HighPrecMoney,
    transactionType :: B.C f Domain.Types.DriverWallet.TransactionType,
    updatedAt :: B.C f Data.Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverWalletT where
  data PrimaryKey DriverWalletT f = DriverWalletId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverWalletId . id

type DriverWallet = DriverWalletT Identity

$(enableKVPG ''DriverWalletT ['id] [])

$(mkTableInstances ''DriverWalletT "driver_wallet")
