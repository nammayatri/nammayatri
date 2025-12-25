{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSVehicleServiceTier where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSVehicleServiceTierT f = FRFSVehicleServiceTierT
  { _type :: (B.C f BecknV2.FRFS.Enums.ServiceTierType),
    description :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    integratedBppConfigId :: (B.C f Kernel.Prelude.Text),
    isAirConditioned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    longName :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    providerCode :: (B.C f Kernel.Prelude.Text),
    shortName :: (B.C f Kernel.Prelude.Text),
    trainType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSVehicleServiceTierT where
  data PrimaryKey FRFSVehicleServiceTierT f = FRFSVehicleServiceTierId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSVehicleServiceTierId . id

type FRFSVehicleServiceTier = FRFSVehicleServiceTierT Identity

$(enableKVPG (''FRFSVehicleServiceTierT) [('id)] [])

$(mkTableInstances (''FRFSVehicleServiceTierT) "frfs_vehicle_service_tier")
