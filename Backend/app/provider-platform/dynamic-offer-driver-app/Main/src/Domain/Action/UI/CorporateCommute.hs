{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CorporateCommute
  ( getDriverCorporateShifts,
    getDriverCorporateRoutes,
    getDriverCorporateRoster,
    postDriverCorporateRideAccept,
    getDriverCorporateEarnings,
  )
where

import qualified API.Types.UI.CorporateCommute as API
import Data.Time.Calendar (Day)
import qualified Domain.Types.CorporateShift as DCS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

-- | List corporate shifts available to the driver
getDriverCorporateShifts ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m [API.CorporateShiftDriverResp]
getDriverCorporateShifts (_personId, _merchantId, _merchantOperatingCityId) = do
  throwError (InvalidRequest "Driver corporate shifts listing is not yet implemented")

-- | List routes for a given corporate shift
getDriverCorporateRoutes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DCS.CorporateShift ->
  m [API.CorporateRouteDriverResp]
getDriverCorporateRoutes (_personId, _merchantId, _merchantOperatingCityId) shiftId = do
  logInfo $ "Corporate Commute: driver requesting routes for shift " <> shiftId.getId <> " - stub"
  pure []

-- | Get roster entries assigned to the driver for a given date
getDriverCorporateRoster ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  m [API.CorporateRosterDriverResp]
getDriverCorporateRoster (_personId, _merchantId, _merchantOperatingCityId) _date = do
  logInfo "Corporate Commute: driver requesting roster - stub"
  pure []

-- | Accept a corporate ride assignment
postDriverCorporateRideAccept ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  API.AcceptCorporateRideReq ->
  m APISuccess.APISuccess
postDriverCorporateRideAccept (_personId, _merchantId, _merchantOperatingCityId) req = do
  logInfo $ "Corporate Commute: driver accepting corporate ride for roster " <> req.rosterId.getId <> " - stub"
  pure APISuccess.Success

-- | Get corporate earnings summary for the driver
getDriverCorporateEarnings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m API.CorporateEarningsSummary
getDriverCorporateEarnings (_personId, _merchantId, _merchantOperatingCityId) = do
  logInfo "Corporate Commute: driver requesting earnings summary - stub"
  now <- getCurrentTime
  pure $
    API.CorporateEarningsSummary
      { totalTrips = 0,
        totalEarnings = 0,
        periodStart = now,
        periodEnd = now
      }
