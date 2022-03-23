module Beckn.Serviceability where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Types.Geofencing
import Beckn.Types.MapSearch

rideServiceable ::
  (EsqDBFlow m r, HasInConfig r c "geofencingConfig" GeofencingConfig) =>
  (LatLong -> [Text] -> m Bool) ->
  LatLong ->
  LatLong ->
  m Bool
rideServiceable someGeometriesContain origin destination = do
  geofencingConfig <- askConfig (.geofencingConfig)
  originServiceable <-
    case geofencingConfig.origin of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain origin regions
  destinationServiceable <-
    case geofencingConfig.destination of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain destination regions
  pure $ originServiceable && destinationServiceable
