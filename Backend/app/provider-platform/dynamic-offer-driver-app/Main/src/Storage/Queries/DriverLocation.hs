{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Storage.Tabular.DriverLocation

create :: Id Person -> LatLong -> UTCTime -> Id Merchant -> SqlDB ()
create drLocationId latLong updateTime merchantId = do
  -- Tricky query to be able to insert meaningful Point
  now <- getCurrentTime
  Esq.insertSelect $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val updateTime
        <#> val now
        <#> val now
        <#> val (toKey merchantId)

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> Id Merchant -> SqlDB DriverLocation
upsertGpsCoord drLocationId latLong calculationTime merchantId = do
  now <- getCurrentTime
  let locationObject =
        DriverLocation
          { driverId = drLocationId,
            lat = latLong.lat,
            lon = latLong.lon,
            coordinatesCalculatedAt = calculationTime,
            createdAt = now,
            updatedAt = now,
            merchantId
          }
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverLocationLat =. val latLong.lat,
        DriverLocationLon =. val latLong.lon,
        DriverLocationCoordinatesCalculatedAt =. val calculationTime,
        DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon),
        DriverLocationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)
  return locationObject

deleteById :: Id Person -> SqlDB ()
deleteById = deleteByKey @DriverLocationT
