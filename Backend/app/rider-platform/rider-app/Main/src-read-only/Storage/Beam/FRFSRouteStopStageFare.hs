{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSRouteStopStageFare where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSRouteStopStageFareT f = FRFSRouteStopStageFareT
  { farePolicyId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    routeCode :: (B.C f Kernel.Prelude.Text),
    stage :: (B.C f Kernel.Prelude.Int),
    stopCode :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSRouteStopStageFareT where
  data PrimaryKey FRFSRouteStopStageFareT f = FRFSRouteStopStageFareId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSRouteStopStageFareId <$> farePolicyId <*> routeCode <*> stopCode

type FRFSRouteStopStageFare = FRFSRouteStopStageFareT Identity

$(enableKVPG (''FRFSRouteStopStageFareT) [('farePolicyId), ('routeCode), ('stopCode)] [])

$(mkTableInstances (''FRFSRouteStopStageFareT) "frfs_route_stop_stage_fare")
