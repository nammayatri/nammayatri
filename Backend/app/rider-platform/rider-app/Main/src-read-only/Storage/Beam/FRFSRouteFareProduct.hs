{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSRouteFareProduct where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data FRFSRouteFareProductT f = FRFSRouteFareProductT
  { farePolicyId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    vehicleServiceTierId :: B.C f Kernel.Prelude.Text,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSRouteFareProductT where
  data PrimaryKey FRFSRouteFareProductT f = FRFSRouteFareProductId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSRouteFareProductId . id

type FRFSRouteFareProduct = FRFSRouteFareProductT Identity

$(enableKVPG ''FRFSRouteFareProductT ['id] [])

$(mkTableInstances ''FRFSRouteFareProductT "frfs_route_fare_product")
