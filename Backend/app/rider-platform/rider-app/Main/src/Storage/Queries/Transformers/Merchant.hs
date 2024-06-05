{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Merchant (module Storage.Queries.Transformers.Merchant, module Reexport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Geofencing
import qualified Kernel.Types.Geofencing
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.Transformers.Distance as Reexport

mkGeofencingConfig :: (Kernel.Types.Geofencing.GeoRestriction -> Kernel.Types.Geofencing.GeoRestriction -> Kernel.Types.Geofencing.GeofencingConfig)
mkGeofencingConfig destinationRestriction originRestriction =
  GeofencingConfig
    { origin = originRestriction,
      destination = destinationRestriction
    }
