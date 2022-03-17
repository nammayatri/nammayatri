module Product.Serviceability where

import App.Types
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Domain.Types.Person as Person
import EulerHS.Prelude hiding (length)
import Storage.Queries.Geometry (someGeometriesContain)
import Types.API.Serviceability
import Utils.Common

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
      Regions regions -> someGeometriesContain location regions
  pure $ ServiceabilityRes locationServiceable
