module Domain.Action.UI.Serviceability
  ( checkServiceability,
  )
where

import Domain.Types.Person as Person
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.Queries.Geometry (someGeometriesContain)
import qualified Storage.Queries.Person as QP
import Tools.Error

checkServiceability ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
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
