module Product.Serviceability where

import App.Types
import Beckn.Types.Id
import EulerHS.Prelude hiding (length)
import Storage.Queries.Geometry (someGeometriesContain)
import Types.API.Serviceability
import Types.Geofencing
import Types.Storage.Person as Person
import Utils.Common

rideServiceable :: (DBFlow m r, HasFlowEnv m r '["geofencingConfig" ::: GeofencingConfig]) => RideServiceabilityReq -> m Bool
rideServiceable RideServiceabilityReq {..} = do
  geofencingConfig <- asks (.geofencingConfig)
  originServiceable <-
    case geofencingConfig.origin of
      Unrestricted -> pure True
      Region region -> someGeometriesContain origin region
  destinationServiceable <-
    case geofencingConfig.destination of
      Unrestricted -> pure True
      Region region -> someGeometriesContain destination region
  pure $ originServiceable && destinationServiceable

checkServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  Id Person.Person ->
  ServiceabilityReq ->
  FlowHandler ServiceabilityRes
checkServiceability settingAccessor personId ServiceabilityReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  geoRestriction <- asks $ settingAccessor . geofencingConfig
  locationServiceable <-
    case geoRestriction of
      Unrestricted -> pure True
      Region region -> someGeometriesContain location region
  pure $ ServiceabilityRes locationServiceable
