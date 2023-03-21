{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Serviceability
  ( checkServiceability,
  )
where

import Domain.Types.Person as Person
import Domain.Types.Serviceability as Serviceability
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as DSpecialLocation
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
  m Serviceability.ServiceabilityRes
checkServiceability settingAccessor personId location = do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  let merchId = person.merchantId
  geoConfig <- fmap (.geofencingConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  case geoRestriction of
    Unrestricted -> do
      let serviceable = True
      specialLocationBody <- checkIfSpecialLocation location
      pure Serviceability.ServiceabilityRes {serviceable = serviceable, specialLocation = specialLocationBody}
    Regions regions -> do
      serviceable <- someGeometriesContain location regions
      if serviceable
        then do
          specialLocationBody <- checkIfSpecialLocation location
          pure Serviceability.ServiceabilityRes {serviceable = serviceable, specialLocation = specialLocationBody}
        else pure Serviceability.ServiceabilityRes {serviceable = serviceable, specialLocation = Nothing}

isSpecialZone :: [DSpecialLocation.SpecialLocation] -> Bool
isSpecialZone [] = False
isSpecialZone (_ : _) = True

checkIfSpecialLocation ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  LatLong ->
  m (Maybe [DSpecialLocation.SpecialLocation])
checkIfSpecialLocation location = do
  specialLocations <- QSpecialLocation.findSpecialLocationByLatLong location
  if isSpecialZone specialLocations
    then do
      return $ Just specialLocations
    else return Nothing
