{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Merchant where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Geofencing
import qualified Kernel.Types.Geofencing
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)

mkGeofencingConfig :: (Kernel.Types.Geofencing.GeoRestriction -> Kernel.Types.Geofencing.GeoRestriction -> Kernel.Types.Geofencing.GeofencingConfig)
mkGeofencingConfig destinationRestriction originRestriction =
  GeofencingConfig
    { origin = originRestriction,
      destination = destinationRestriction
    }
