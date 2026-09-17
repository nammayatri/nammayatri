{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverFyEarnings where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverFyEarningsT f = DriverFyEarningsT
  { financialYear :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    netEarningsTotal :: (B.C f Kernel.Types.Common.HighPrecMoney),
    personId :: (B.C f Kernel.Prelude.Text),
    quarter :: (B.C f Kernel.Prelude.Int),
    tdsAmountTotal :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFyEarningsT where
  data PrimaryKey DriverFyEarningsT f = DriverFyEarningsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverFyEarningsId . id

type DriverFyEarnings = DriverFyEarningsT Identity

$(enableKVPG (''DriverFyEarningsT) [('id)] [[('personId)]])

$(mkTableInstances (''DriverFyEarningsT) "driver_fy_earnings")
