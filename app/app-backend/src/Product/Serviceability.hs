module Product.Serviceability where

import App.Types
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Domain.Types.Person as Person
import EulerHS.Prelude hiding (length)
import Storage.Queries.Geometry (someGeometriesContain)
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QP
import Types.API.Serviceability
import Types.Error
import Utils.Common

checkServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  Id Person.Person ->
  ServiceabilityReq ->
  FlowHandler ServiceabilityRes
checkServiceability settingAccessor personId ServiceabilityReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  let merchId = person.merchantId
  geoConfig <- fmap (.geofencingConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  locationServiceable <-
    case geoRestriction of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain location regions
  pure $ ServiceabilityRes locationServiceable
