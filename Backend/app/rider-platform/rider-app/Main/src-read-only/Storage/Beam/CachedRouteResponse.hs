{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CachedRouteResponse where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data CachedRouteResponseT f = CachedRouteResponseT
  { avoidToll :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    distance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    dropGeohash :: (B.C f Kernel.Prelude.Text),
    duration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    hourOfDay :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    pickupGeohash :: (B.C f Kernel.Prelude.Text),
    riderId :: (B.C f Kernel.Prelude.Text),
    routes :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table CachedRouteResponseT where
  data PrimaryKey CachedRouteResponseT f = CachedRouteResponseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CachedRouteResponseId . id

type CachedRouteResponse = CachedRouteResponseT Identity

$(enableKVPG (''CachedRouteResponseT) [('id)] [[('avoidToll), ('dropGeohash), ('hourOfDay), ('pickupGeohash), ('riderId)]])

$(mkTableInstances (''CachedRouteResponseT) "cached_route_response")
