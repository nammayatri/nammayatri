{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RecentLocation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.RecentLocation
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data RecentLocationT f
    = RecentLocationT {address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       entityType :: (B.C f Domain.Types.RecentLocation.EntityType),
                       fare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                       frequency :: (B.C f Kernel.Prelude.Int),
                       fromGeohash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       stopLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                       stopLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                       fromStopCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       id :: (B.C f Kernel.Prelude.Text),
                       merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                       riderId :: (B.C f Kernel.Prelude.Text),
                       routeCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       toGeohash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       lat :: (B.C f Kernel.Prelude.Double),
                       lon :: (B.C f Kernel.Prelude.Double),
                       stopCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RecentLocationT
    where data PrimaryKey RecentLocationT f = RecentLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RecentLocationId . id
type RecentLocation = RecentLocationT Identity

$(enableKVPG (''RecentLocationT) [('id)] [[('riderId)]])

$(mkTableInstances (''RecentLocationT) "recent_location")

