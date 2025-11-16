{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ParkingTransaction where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ParkingTransaction
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data ParkingTransactionT f = ParkingTransactionT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    endTime :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    parkingLotId :: (B.C f Kernel.Prelude.Text),
    paymentOrderId :: (B.C f Kernel.Prelude.Text),
    startTime :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Domain.Types.ParkingTransaction.StatusType),
    vehicleNumber :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ParkingTransactionT where
  data PrimaryKey ParkingTransactionT f = ParkingTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ParkingTransactionId . id

type ParkingTransaction = ParkingTransactionT Identity

$(enableKVPG (''ParkingTransactionT) [('id)] [])

$(mkTableInstances (''ParkingTransactionT) "parking_transaction")
