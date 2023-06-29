{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverLocation.Internal where

import qualified Data.Maybe as Mb
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Value)
import Storage.Tabular.DriverLocation

getDriverLocsWithMerchantId ::
  Transactionable m =>
  [Id Person] ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocsWithMerchantId driverIds merchantId = do
  Esq.findAll $ do
    driverLocs <- from $ table @DriverLocationT
    where_ $
      driverLocs ^. DriverLocationDriverId `in_` valList (toKey <$> driverIds)
        &&. driverLocs ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
    return driverLocs

getDriverLocsNearby ::
  (Transactionable m, MonadTime m) =>
  Id Merchant ->
  [Id Person] ->
  Maybe Seconds ->
  LatLong ->
  Meters ->
  m [DriverLocation]
getDriverLocsNearby merchantId driverIds mbDriverPositionInfoExpiry LatLong {..} radiusMeters = do
  now <- getCurrentTime
  Esq.findAll $ do
    driverLocs <- from $ table @DriverLocationT
    where_ $
      driverLocs ^. DriverLocationDriverId `in_` valList (toKey <$> driverIds)
        &&. driverLocs ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
        &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                ||. (driverLocs ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
            )
        &&. buildRadiusWithin (driverLocs ^. DriverLocationPoint) (lat, lon) (val radiusMeters.getMeters)
    orderBy [asc (driverLocs ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon))]
    return driverLocs

getAllDriverLocsNearby ::
  (Transactionable m, MonadTime m) =>
  Id Merchant ->
  Maybe Seconds ->
  LatLong ->
  Meters ->
  m [DriverLocation]
getAllDriverLocsNearby merchantId mbDriverPositionInfoExpiry LatLong {..} radiusMeters = do
  now <- getCurrentTime
  Esq.findAll $ do
    driverLocs <- from $ table @DriverLocationT
    where_ $
      driverLocs ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
        &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                ||. (driverLocs ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
            )
        &&. buildRadiusWithin (driverLocs ^. DriverLocationPoint) (lat, lon) (val radiusMeters.getMeters)
    orderBy [asc (driverLocs ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon))]
    return driverLocs
