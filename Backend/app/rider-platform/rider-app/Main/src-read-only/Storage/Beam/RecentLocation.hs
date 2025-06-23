{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RecentLocation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RecentLocation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RecentLocationT f = RecentLocationT
  { address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    entityType :: B.C f Domain.Types.RecentLocation.EntityType,
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    frequency :: B.C f Kernel.Prelude.Int,
    fromGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    stopLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    stopLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    fromStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    riderId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    stopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RecentLocationT where
  data PrimaryKey RecentLocationT f = RecentLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RecentLocationId . id

type RecentLocation = RecentLocationT Identity

$(enableKVPG ''RecentLocationT ['id] [['riderId]])

$(mkTableInstances ''RecentLocationT "recent_location")
