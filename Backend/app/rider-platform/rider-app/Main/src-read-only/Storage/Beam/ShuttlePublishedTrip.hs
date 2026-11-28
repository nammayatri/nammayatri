{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ShuttlePublishedTrip where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ShuttlePublishedTripT f = ShuttlePublishedTripT
  { createdAt :: (B.C f Data.Time.UTCTime),
    day :: (B.C f Kernel.Prelude.Text),
    departure :: (B.C f Kernel.Prelude.Text),
    feedKey :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    riders :: (B.C f Kernel.Prelude.Int),
    routeCode :: (B.C f Kernel.Prelude.Text),
    tripId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Data.Time.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ShuttlePublishedTripT where
  data PrimaryKey ShuttlePublishedTripT f = ShuttlePublishedTripId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ShuttlePublishedTripId <$> day <*> feedKey <*> tripId

type ShuttlePublishedTrip = ShuttlePublishedTripT Identity

$(enableKVPG (''ShuttlePublishedTripT) [('day), ('feedKey), ('tripId)] [])

$(mkTableInstances (''ShuttlePublishedTripT) "shuttle_published_trip")
