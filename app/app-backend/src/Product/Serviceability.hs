module Product.Serviceability where

import App.Types
import Beckn.Types.Storage.Person as Person
import EulerHS.Prelude hiding (length)
import Storage.Queries.Geometry (someGeometriesContain)
import Types.API.Serviceability
import Types.Geofencing
import Utils.Common

rideServiceable :: (HasFlowDBEnv m r, HasFlowEnv m r '["geofencingConfig" ::: GeofencingConfig]) => RideServiceabilityReq -> m Bool
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

checkRideServiceability :: Person.Person -> RideServiceabilityReq -> FlowHandler RideServiceabilityRes
checkRideServiceability _ req =
  withFlowHandlerAPI $
    RideServiceabilityRes <$> rideServiceable req

checkServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  Person.Person ->
  ServiceabilityReq ->
  FlowHandler ServiceabilityRes
checkServiceability settingAccessor _ ServiceabilityReq {..} = withFlowHandlerAPI $ do
  geoRestriction <- asks $ settingAccessor . geofencingConfig
  locationServiceable <-
    case geoRestriction of
      Unrestricted -> pure True
      Region region -> someGeometriesContain location region
  pure $ ServiceabilityRes locationServiceable
