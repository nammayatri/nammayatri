{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverLocation.Internal where

import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant
import Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Value)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT

getDriverLocsWithCond ::
  (MonadFlow m, MonadTime m, MonadReader r m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Maybe Seconds ->
  LatLong ->
  Meters ->
  m [DriverLocation]
getDriverLocsWithCond merchantId _mbDriverPositionInfoExpiry LatLong {..} radiusMeters =
  LF.nearBy lat lon Nothing Nothing radiusMeters.getMeters merchantId
