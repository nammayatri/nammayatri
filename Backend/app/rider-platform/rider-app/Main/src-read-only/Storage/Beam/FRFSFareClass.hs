{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSFareClass where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data FRFSFareClassT f = FRFSFareClassT
  { classCode :: (B.C f Kernel.Prelude.Text),
    displayName :: (B.C f Kernel.Prelude.Text),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    routeTag :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    timeBounds :: (B.C f Kernel.Types.TimeBound.TimeBound),
    vehicleServiceTierId :: (B.C f Kernel.Prelude.Text),
    vehicleTag :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleVariant :: (B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.BusVehicleVariant)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSFareClassT where
  data PrimaryKey FRFSFareClassT f = FRFSFareClassId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSFareClassId . id

type FRFSFareClass = FRFSFareClassT Identity

$(enableKVPG (''FRFSFareClassT) [('id)] [])

$(mkTableInstances (''FRFSFareClassT) "frfs_fare_class")
