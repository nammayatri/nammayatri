module Beckn.Serviceability where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Geofencing
import Beckn.Types.MapSearch

rideServiceable ::
  (EsqDBFlow m r, HasField "geofencingConfig" r GeofencingConfig) =>
  (LatLong -> [Text] -> m Bool) ->
  LatLong ->
  Maybe LatLong ->
  m Bool
rideServiceable someGeometriesContain origin mbDestination = do
  geofencingConfig <- asks (.geofencingConfig)
  originServiceable <-
    case geofencingConfig.origin of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain origin regions
  destinationServiceable <-
    case geofencingConfig.destination of
      Unrestricted -> pure True
      Regions regions -> do
        maybe (pure True) (`someGeometriesContain` regions) mbDestination
  pure $ originServiceable && destinationServiceable
