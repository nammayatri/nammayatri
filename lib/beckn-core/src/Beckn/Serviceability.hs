module Beckn.Serviceability where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.App
import Beckn.Types.Field
import Beckn.Types.Geofencing
import Beckn.Types.MapSearch

rideServiceable ::
  (EsqDBFlow m r, HasFlowEnv m r '["geofencingConfig" ::: GeofencingConfig]) =>
  (LatLong -> [Text] -> m Bool) ->
  LatLong ->
  LatLong ->
  m Bool
rideServiceable someGeometriesContain origin destination = do
  geofencingConfig <- asks (.geofencingConfig)
  originServiceable <-
    case geofencingConfig.origin of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain origin regions
  destinationServiceable <-
    case geofencingConfig.destination of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain destination regions
  pure $ originServiceable && destinationServiceable
