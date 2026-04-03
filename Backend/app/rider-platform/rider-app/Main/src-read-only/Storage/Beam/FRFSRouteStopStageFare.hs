{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FRFSRouteStopStageFare where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FRFSRouteStopStageFareT f
    = FRFSRouteStopStageFareT {farePolicyId :: (B.C f Kernel.Prelude.Text),
                               merchantId :: (B.C f Kernel.Prelude.Text),
                               merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                               routeCode :: (B.C f Kernel.Prelude.Text),
                               stage :: (B.C f Kernel.Prelude.Int),
                               stopCode :: (B.C f Kernel.Prelude.Text),
                               createdAt :: (B.C f Kernel.Prelude.UTCTime),
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FRFSRouteStopStageFareT
    where data PrimaryKey FRFSRouteStopStageFareT f = FRFSRouteStopStageFareId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FRFSRouteStopStageFareId <$> farePolicyId <*> routeCode <*> stopCode
type FRFSRouteStopStageFare = FRFSRouteStopStageFareT Identity

$(enableKVPG (''FRFSRouteStopStageFareT) [('farePolicyId), ('routeCode), ('stopCode)] [])

$(mkTableInstances (''FRFSRouteStopStageFareT) "frfs_route_stop_stage_fare")

