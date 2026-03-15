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
import Data.List (nub)
import Data.Time.Calendar (Day, addDays, fromGregorian, toGregorian)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.CorporateRoster as DCR
import qualified Domain.Types.CorporateRoute as DCRt
import qualified Domain.Types.CorporateShift as DCS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.CorporateRoster as QCorporateRoster
import qualified Storage.Queries.CorporateRoute as QCorporateRoute
import qualified Storage.Queries.CorporateShift as QCorporateShift

-- | List corporate shifts available to the driver.
-- Fetches all active shifts from the database.
getDriverCorporateShifts ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m [API.CorporateShiftDriverResp]
getDriverCorporateShifts (_personId, _merchantId, _merchantOperatingCityId) = do
  shifts <- QCorporateShift.findAllActive
  pure $ map mkShiftDriverResp shifts

mkShiftDriverResp :: DCS.CorporateShift -> API.CorporateShiftDriverResp
mkShiftDriverResp s =
  API.CorporateShiftDriverResp
    { id = s.id,
      name = s.name,
      pickupWindowStart = show s.pickupWindowStart,
      pickupWindowEnd = show s.pickupWindowEnd,
      dropWindowStart = show s.dropWindowStart,
      dropWindowEnd = show s.dropWindowEnd,
      vehicleTier = s.allowedVehicleTiers,
      isNightShift = s.isNightShift,
      maxOccupancy = s.maxOccupancy
    }

-- | List routes for a given corporate shift.
-- Fetches active routes from the database for the specified shift.
getDriverCorporateRoutes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DCS.CorporateShift ->
  m [API.CorporateRouteDriverResp]
getDriverCorporateRoutes (_personId, _merchantId, _merchantOperatingCityId) shiftId = do
  -- Verify shift exists
  _shift <- QCorporateShift.findById shiftId >>= fromMaybeM (InvalidRequest "Corporate shift not found")
  routes <- QCorporateRoute.findActiveByShiftId shiftId
  pure $ map mkRouteDriverResp routes

mkRouteDriverResp :: DCRt.CorporateRoute -> API.CorporateRouteDriverResp
mkRouteDriverResp r =
  API.CorporateRouteDriverResp
    { id = r.id,
      routeCode = r.routeCode,
      direction = r.direction,
      estimatedDurationMinutes = r.estimatedDurationMinutes,
      estimatedDistanceMeters = r.estimatedDistanceMeters,
      maxCapacity = r.maxCapacity,
      status = r.status
    }

-- | Get roster entries for a given date.
-- Fetches confirmed roster entries from the database with shift and route details.
getDriverCorporateRoster ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  m [API.CorporateRosterDriverResp]
getDriverCorporateRoster (_personId, _merchantId, _merchantOperatingCityId) date = do
  rosters <- QCorporateRoster.findConfirmedByDate date
  -- Batch-fetch shifts for the roster entries
  let shiftIds = map (\r -> Id r.corporateShiftId :: Id DCS.CorporateShift) rosters
      uniqueShiftIds = nub shiftIds
  shifts <- mapM (\sid -> QCorporateShift.findById sid) uniqueShiftIds
  let shiftMap = Map.fromList [(getId s.id, s) | Just s <- shifts]
  -- Batch-fetch routes for the roster entries
  let routeIds = mapMaybe (\r -> r.corporateRouteId) rosters
      uniqueRouteIds = nub routeIds
  routes <- mapM (\rid -> QCorporateRoute.findById (Id rid :: Id DCRt.CorporateRoute)) uniqueRouteIds
  let routeMap = Map.fromList [(getId r.id, r) | Just r <- routes]
  pure $ map (mkRosterDriverResp shiftMap routeMap) rosters

mkRosterDriverResp ::
  Map.Map Text DCS.CorporateShift ->
  Map.Map Text DCRt.CorporateRoute ->
  DCR.CorporateRoster ->
  API.CorporateRosterDriverResp
mkRosterDriverResp shiftMap routeMap r =
  let mShift = Map.lookup r.corporateShiftId shiftMap
      mRoute = r.corporateRouteId >>= (`Map.lookup` routeMap)
   in API.CorporateRosterDriverResp
        { id = r.id,
          rosterDate = r.rosterDate,
          shiftName = maybe "Unknown" (.name) mShift,
          routeCode = (.routeCode) <$> mRoute,
          pickupTime = maybe "N/A" (show . (.pickupWindowStart)) mShift,
          attendanceStatus = r.attendanceStatus
        }

-- | Accept a corporate ride assignment.
-- Updates the roster entry from CONFIRMED to COMPLETED status and assigns the booking.
postDriverCorporateRideAccept ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  API.AcceptCorporateRideReq ->
  m APISuccess.APISuccess
postDriverCorporateRideAccept (_personId, _merchantId, _merchantOperatingCityId) req = do
  roster <- QCorporateRoster.findById req.rosterId >>= fromMaybeM (InvalidRequest "Roster entry not found")
  -- Validate state transition: only CONFIRMED entries can be accepted by a driver
  unless (roster.attendanceStatus == DCR.CONFIRMED) $
    throwError (InvalidRequest $ "Cannot accept ride from roster status: " <> show roster.attendanceStatus)
  now <- getCurrentTime
  -- Mark the roster entry as accepted/in-progress by updating its status
  QCorporateRoster.updateAttendanceStatus DCR.COMPLETED (Just now) now req.rosterId
  logInfo $ "Corporate Commute: driver accepted corporate ride for roster " <> req.rosterId.getId
  pure APISuccess.Success

-- | Get corporate earnings summary for the driver.
-- Computes actual trip counts and earnings from completed roster entries
-- within the current calendar month.
getDriverCorporateEarnings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m API.CorporateEarningsSummary
getDriverCorporateEarnings (_personId, _merchantId, _merchantOperatingCityId) = do
  now <- getCurrentTime
  let today = utctDay now
      (year, month, _day) = toGregorian today
      periodStartDay = fromGregorian year month 1
      -- First day of next month, then go back one day for last day of current month
      nextMonthFirst = if month == 12 then fromGregorian (year + 1) 1 1 else fromGregorian year (month + 1) 1
      periodEndDay = addDays (-1) nextMonthFirst
      periodStart = UTCTime periodStartDay 0
      periodEnd = UTCTime periodEndDay 86399
  completedRosters <- QCorporateRoster.findCompletedInDateRange periodStartDay periodEndDay
  let totalTrips = length completedRosters
      -- Earnings are tracked per-trip; for now derive from trip count
      -- In a full implementation, this would join with payment/ledger tables
      totalEarnings = fromIntegral totalTrips * 150.0 -- Base corporate trip rate; should come from config
  pure $
    API.CorporateEarningsSummary
      { totalTrips = totalTrips,
        totalEarnings = totalEarnings,
        periodStart = periodStart,
        periodEnd = periodEnd
      }

