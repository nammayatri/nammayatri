{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSCancellationConfig where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSCancellationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSCancellationConfigT f = FRFSCancellationConfigT
  { cancellationChargeType :: (B.C f Domain.Types.FRFSCancellationConfig.CancellationChargeType),
    cancellationChargeValue :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    id :: (B.C f Kernel.Prelude.Text),
    maxMinutesBeforeDeparture :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    minMinutesBeforeDeparture :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f BecknV2.FRFS.Enums.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSCancellationConfigT where
  data PrimaryKey FRFSCancellationConfigT f = FRFSCancellationConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSCancellationConfigId . id

type FRFSCancellationConfig = FRFSCancellationConfigT Identity

$(enableKVPG (''FRFSCancellationConfigT) [('id)] [[('vehicleCategory)]])

$(mkTableInstances (''FRFSCancellationConfigT) "frfs_cancellation_config")
