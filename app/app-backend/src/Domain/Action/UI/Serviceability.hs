module Domain.Action.UI.Serviceability
  ( checkServiceability,
  )
where

import Beckn.Prelude
import Beckn.Product.MapSearch.PolyLinePoints
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Domain.Types.Person as Person
import Storage.Queries.Geometry (someGeometriesContain)
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QP
import Types.Error
import Utils.Common

checkServiceability ::
  EsqDBFlow m r =>
  (GeofencingConfig -> GeoRestriction) ->
  Id Person.Person ->
  LatLong ->
  m Bool
checkServiceability settingAccessor personId location = do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  let merchId = person.merchantId
  geoConfig <- fmap (.geofencingConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  case geoRestriction of
    Unrestricted -> pure True
    Regions regions -> someGeometriesContain location regions
