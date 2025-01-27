{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.DriverGoHome
  ( getDriverGoHomeGetHomeLocation,
    postDriverGoHomeUpdateHomeLocation,
    postDriverGoHomeIncrementGoToCount,
    getDriverGoHomeGetGoHomeInfo,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverGoHome as Common
import qualified Domain.Action.UI.Driver as DDriver
import Domain.Action.UI.DriverGoHomeRequest (CachedGoHomeRequest (..))
import qualified Domain.Types.DriverHomeLocation as DDHL
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverHomeLocation as QDHL

getDriverGoHomeGetHomeLocation ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.GetHomeLocationsRes
getDriverGoHomeGetHomeLocation merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  dghLocs <- DDriver.getHomeLocations (cast driverId, cast merchant.id, merchantOpCityId)
  return (buildDriverHomeLocationAPIEntity <$> dghLocs.locations)
  where
    buildDriverHomeLocationAPIEntity dghLocs =
      Common.DriverHomeLocationAPIEntity
        { id = cast dghLocs.id,
          address = dghLocs.address,
          lat = dghLocs.lat,
          lon = dghLocs.lon,
          tag = dghLocs.tag
        }

---------------------------------------------------------------------
postDriverGoHomeUpdateHomeLocation ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.UpdateDriverHomeLocationReq ->
  Flow APISuccess
postDriverGoHomeUpdateHomeLocation _ _ _ req = do
  QDHL.updateHomeLocationById (cast req.id) buildDriverHomeLocationEntity
  return Success
  where
    buildDriverHomeLocationEntity =
      DDHL.UpdateDriverHomeLocation
        { address = req.address,
          lat = req.lat,
          lon = req.lon,
          tag = req.tag
        }

---------------------------------------------------------------------
postDriverGoHomeIncrementGoToCount ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverGoHomeIncrementGoToCount merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  CQDGR.increaseDriverGoHomeRequestCount merchantOpCityId (cast driverId)
  return Success

---------------------------------------------------------------------
getDriverGoHomeGetGoHomeInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.CachedGoHomeRequestInfoRes
getDriverGoHomeGetGoHomeInfo merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  ghInfo <- CQDGR.getDriverGoHomeRequestInfo (cast driverId) merchantOpCityId Nothing
  return (buildCachedGoHomeRequestInfoRes ghInfo)
  where
    buildCachedGoHomeRequestInfoRes CachedGoHomeRequest {..} =
      Common.CachedGoHomeRequestInfoRes
        { status = show <$> status,
          driverGoHomeRequestId = cast <$> driverGoHomeRequestId,
          ..
        }
