module Storage.Queries.Transformers.Merchant where

import Kernel.Types.Geofencing

mkGeofencingConfig :: (Kernel.Types.Geofencing.GeoRestriction -> Kernel.Types.Geofencing.GeoRestriction -> Kernel.Types.Geofencing.GeofencingConfig)
mkGeofencingConfig destinationRestriction originRestriction =
  GeofencingConfig
    { origin = originRestriction,
      destination = destinationRestriction
    }
