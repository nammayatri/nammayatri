{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RecentLocation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RecentLocation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RecentLocationT f = RecentLocationT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    entityId :: (B.C f Kernel.Prelude.Text),
    entityType :: (B.C f Domain.Types.RecentLocation.EntityType),
    id :: (B.C f Kernel.Prelude.Text),
    lat :: (B.C f Kernel.Prelude.Double),
    lon :: (B.C f Kernel.Prelude.Double),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    riderId :: (B.C f Kernel.Prelude.Text),
    routeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RecentLocationT where
  data PrimaryKey RecentLocationT f = RecentLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RecentLocationId . id

type RecentLocation = RecentLocationT Identity

$(enableKVPG (''RecentLocationT) [('id)] [[('riderId)]])

$(mkTableInstances (''RecentLocationT) "recent_location")
