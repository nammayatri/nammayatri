{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.NearbyDrivers (postNearbyDrivers) where

import qualified API.Types.UI.NearbyDrivers as ND
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleVariant as DV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified Storage.CachedQueries.Merchant as CQM

postNearbyDrivers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    ND.NearbyDriverReq ->
    Environment.Flow ND.NearbyDriverRes
  )
postNearbyDrivers (_, merchantId) req = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  nearbyDriverLocations <- LF.nearBy req.location.lat req.location.lon Nothing (Just [DV.AUTO_RICKSHAW, DV.SEDAN, DV.SUV, DV.HATCHBACK, DV.TAXI, DV.TAXI_PLUS, DV.PREMIUM_SEDAN, DV.BLACK, DV.BLACK_XL, DV.SUV_PLUS]) req.radius merchant.driverOfferMerchantId
  return $
    ND.NearbyDriverRes $
      map
        ( \driverLocation ->
            ND.NearbyDriver
              { id = driverLocation.driverId,
                vehicleType = driverLocation.vehicleType,
                location = LatLong {lat = driverLocation.lat, lon = driverLocation.lon},
                bearing = driverLocation.bear
              }
        )
        nearbyDriverLocations
