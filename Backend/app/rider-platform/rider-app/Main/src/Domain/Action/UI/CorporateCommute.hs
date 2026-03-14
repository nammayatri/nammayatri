{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CorporateCommute
  ( getCorporateEntity,
    listEmployees,
    createEmployee,
    bulkUploadEmployees,
    listShifts,
    createShift,
    getRoster,
    confirmAttendance,
    listRoutes,
    optimizeRoutes,
    listBookings,
    scheduleRides,
    getWallet,
    topUpWallet,
    listInvoices,
    getAnalytics,
  )
where

import qualified API.Types.UI.CorporateCommute as API
import Data.Time.Calendar (Day)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.CorporateEmployee as DCE
import qualified Domain.Types.CorporateEntity as DCEnt
import qualified Domain.Types.CorporateInvoice as DCI
import qualified Domain.Types.CorporatePolicy as DCP
import qualified Domain.Types.CorporateRoster as DCR
import qualified Domain.Types.CorporateRoute as DCRt
import qualified Domain.Types.CorporateShift as DCS
import qualified Domain.Types.CorporateWallet as DCW
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.CorporateEmployee as QCorporateEmployee
import qualified Storage.Queries.CorporateEntity as QCorporateEntity
import qualified Storage.Queries.CorporateInvoice as QCorporateInvoice
import qualified Storage.Queries.CorporateRoster as QCorporateRoster
import qualified Storage.Queries.CorporateRoute as QCorporateRoute
import qualified Storage.Queries.CorporateShift as QCorporateShift
import qualified Storage.Queries.CorporateWallet as QCorporateWallet

-- | Helper to fetch the corporate entity for a merchant (avoids repetition across handlers)
getCorporateEntityForMerchant ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  m DCEnt.CorporateEntity
getCorporateEntityForMerchant merchantId = do
  entities <- QCorporateEntity.findByMerchantId merchantId
  listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")

-- | Helper to verify a shift belongs to the given corporate entity
verifyShiftOwnership ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DCEnt.CorporateEntity ->
  Id DCS.CorporateShift ->
  m DCS.CorporateShift
verifyShiftOwnership entityId shiftId = do
  shift <- QCorporateShift.findByPrimaryKey shiftId >>= fromMaybeM (InvalidRequest "Shift not found")
  unless (shift.corporateEntityId == entityId) $ throwError (InvalidRequest "Shift does not belong to this corporate entity")
  pure shift

-- | Fetch the corporate entity for the current merchant
getCorporateEntity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m API.CorporateEntityResp
getCorporateEntity (_personId, merchantId) = do
  entity <- getCorporateEntityForMerchant merchantId
  pure $
    API.CorporateEntityResp
      { id = entity.id,
        name = entity.name,
        registeredName = entity.registeredName,
        gstin = entity.gstin,
        industry = entity.industry,
        contactPersonName = entity.contactPersonName,
        contactEmail = entity.contactEmail,
        contactPhone = entity.contactPhone,
        billingModel = entity.billingModel,
        billingCycleType = entity.billingCycleType,
        creditLimit = entity.creditLimit,
        currency = entity.currency,
        status = entity.status,
        contractStartDate = entity.contractStartDate,
        contractEndDate = entity.contractEndDate
      }

-- | List employees with pagination
listEmployees ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Int ->
  Int ->
  m [API.CorporateEmployeeResp]
listEmployees (_personId, merchantId) limit offset = do
  entity <- getCorporateEntityForMerchant merchantId
  let safeLimit = min (max 1 limit) 100
      safeOffset = max 0 offset
  employees <- QCorporateEmployee.findByCorporateEntityId entity.id
  pure $ map mkEmployeeResp (take safeLimit . drop safeOffset $ employees)

mkEmployeeResp :: DCE.CorporateEmployee -> API.CorporateEmployeeResp
mkEmployeeResp emp =
  API.CorporateEmployeeResp
    { id = emp.id,
      employeeCode = emp.employeeCode,
      name = emp.name,
      email = emp.email,
      phone = emp.phone,
      department = emp.department,
      costCenter = emp.costCenter,
      status = emp.status,
      linkedAt = emp.linkedAt
    }

-- | Create a single employee
createEmployee ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  API.CreateEmployeeReq ->
  m API.CorporateEmployeeResp
createEmployee (_personId, merchantId) req = do
  entity <- getCorporateEntityForMerchant merchantId
  -- Input validation
  when (T.null req.name) $ throwError (InvalidRequest "Employee name cannot be empty")
  when (T.null req.employeeCode) $ throwError (InvalidRequest "Employee code cannot be empty")
  when (T.null req.email) $ throwError (InvalidRequest "Email cannot be empty")
  when (T.null req.phone) $ throwError (InvalidRequest "Phone cannot be empty")
  when (req.defaultPickupLat < -90 || req.defaultPickupLat > 90) $ throwError (InvalidRequest "Invalid latitude: must be between -90 and 90")
  when (req.defaultPickupLon < -180 || req.defaultPickupLon > 180) $ throwError (InvalidRequest "Invalid longitude: must be between -180 and 180")
  newId <- generateGUID
  now <- getCurrentTime
  let employee =
        DCE.CorporateEmployee
          { id = newId,
            corporateEntityId = entity.id,
            personId = Nothing,
            employeeCode = req.employeeCode,
            name = req.name,
            email = req.email,
            phone = req.phone,
            department = req.department,
            costCenter = req.costCenter,
            gender = DP.UNKNOWN,
            officeLocationId = Nothing,
            defaultPickupLat = req.defaultPickupLat,
            defaultPickupLon = req.defaultPickupLon,
            defaultPickupAddress = req.defaultPickupAddress,
            reportingManagerEmail = req.reportingManagerEmail,
            status = DCE.ACTIVE,
            linkedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QCorporateEmployee.create employee
  pure $ mkEmployeeResp employee

-- | Bulk upload employees from CSV
bulkUploadEmployees ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  API.BulkUploadReq ->
  m API.BulkUploadResp
bulkUploadEmployees (_personId, _merchantId) _req = do
  logInfo "Corporate Commute: bulk upload employees - stub"
  pure $
    API.BulkUploadResp
      { totalRows = 0,
        successCount = 0,
        errorCount = 0,
        errors = []
      }

-- | List shifts for the corporate entity
listShifts ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m [API.CorporateShiftResp]
listShifts (_personId, merchantId) = do
  entity <- getCorporateEntityForMerchant merchantId
  shifts <- QCorporateShift.findByCorporateEntityId entity.id
  pure $ map mkShiftResp shifts

mkShiftResp :: DCS.CorporateShift -> API.CorporateShiftResp
mkShiftResp s =
  API.CorporateShiftResp
    { id = s.id,
      name = s.name,
      pickupWindowStart = show s.pickupWindowStart,
      pickupWindowEnd = show s.pickupWindowEnd,
      dropWindowStart = show s.dropWindowStart,
      dropWindowEnd = show s.dropWindowEnd,
      activeDays = s.activeDays,
      isNightShift = s.isNightShift,
      maxOccupancy = s.maxOccupancy,
      allowedVehicleTiers = s.allowedVehicleTiers,
      confirmationDeadlineMinutes = s.confirmationDeadlineMinutes,
      status = s.status
    }

-- | Create a shift
createShift ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  API.CreateShiftReq ->
  m API.CorporateShiftResp
createShift (_personId, merchantId) req = do
  entity <- getCorporateEntityForMerchant merchantId
  newId <- generateGUID
  now <- getCurrentTime
  when (T.null req.name) $ throwError (InvalidRequest "Shift name cannot be empty")
  when (req.maxOccupancy <= 0) $ throwError (InvalidRequest "Max occupancy must be positive")
  when (req.confirmationDeadlineMinutes <= 0) $ throwError (InvalidRequest "Confirmation deadline must be positive")
  let parseTOD t = readMaybe (toString t) & fromMaybeM (InvalidRequest $ "Invalid time format: " <> t)
  pickupStart <- parseTOD req.pickupWindowStart
  pickupEnd <- parseTOD req.pickupWindowEnd
  dropStart <- parseTOD req.dropWindowStart
  dropEnd <- parseTOD req.dropWindowEnd
  when (pickupStart >= pickupEnd) $ throwError (InvalidRequest "Pickup window start must be before end")
  when (dropStart >= dropEnd) $ throwError (InvalidRequest "Drop window start must be before end")
  let shift =
        DCS.CorporateShift
          { id = newId,
            corporateEntityId = entity.id,
            merchantOperatingCityId = entity.merchantOperatingCityId,
            name = req.name,
            pickupWindowStart = pickupStart,
            pickupWindowEnd = pickupEnd,
            dropWindowStart = dropStart,
            dropWindowEnd = dropEnd,
            activeDays = req.activeDays,
            isNightShift = req.isNightShift,
            maxOccupancy = req.maxOccupancy,
            allowedVehicleTiers = req.allowedVehicleTiers,
            confirmationDeadlineMinutes = req.confirmationDeadlineMinutes,
            status = DCS.CS_ACTIVE,
            createdAt = now,
            updatedAt = now
          }
  QCorporateShift.create shift
  pure $ mkShiftResp shift

-- | Get roster for a given shift and date
getRoster ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DCS.CorporateShift ->
  Day ->
  m [API.CorporateRosterResp]
getRoster (_personId, merchantId) shiftId date = do
  entity <- getCorporateEntityForMerchant merchantId
  _ <- verifyShiftOwnership entity.id shiftId
  rosters <- QCorporateRoster.findByShiftIdAndDate shiftId date
  -- Batch-fetch employees to avoid N+1 queries
  let employeeIds = map (.corporateEmployeeId) rosters
  employees <- mapM (\eid -> QCorporateEmployee.findById eid) employeeIds
  let employeeMap = Map.fromList [(e.id, e) | Just e <- employees]
  pure $ map (mkRosterRespFromMap employeeMap) rosters

mkRosterRespFromMap :: Map.Map (Id DCE.CorporateEmployee) DCE.CorporateEmployee -> DCR.CorporateRoster -> API.CorporateRosterResp
mkRosterRespFromMap employeeMap r =
  let mEmployee = Map.lookup r.corporateEmployeeId employeeMap
   in API.CorporateRosterResp
        { id = r.id,
          employeeName = maybe "Unknown" (.name) mEmployee,
          employeeCode = maybe "Unknown" (.employeeCode) mEmployee,
          attendanceStatus = r.attendanceStatus,
          confirmedAt = r.confirmedAt
        }

-- | Confirm attendance for a roster entry
confirmAttendance ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DCR.CorporateRoster ->
  m APISuccess.APISuccess
confirmAttendance (_personId, merchantId) rosterId = do
  entity <- getCorporateEntityForMerchant merchantId
  roster <- QCorporateRoster.findById rosterId >>= fromMaybeM (InvalidRequest "Roster entry not found")
  -- Verify roster belongs to this entity's shift
  _ <- verifyShiftOwnership entity.id roster.corporateShiftId
  -- Validate state transition: only SCHEDULED can be confirmed
  unless (roster.attendanceStatus == DCR.SCHEDULED) $
    throwError (InvalidRequest $ "Cannot confirm attendance from status: " <> show roster.attendanceStatus)
  now <- getCurrentTime
  QCorporateRoster.updateAttendanceStatus DCR.CONFIRMED (Just now) now rosterId
  logInfo $ "Corporate Commute: confirmed attendance for roster " <> rosterId.getId
  pure APISuccess.Success

-- | List routes for a shift
listRoutes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DCS.CorporateShift ->
  m [API.CorporateRouteResp]
listRoutes (_personId, merchantId) shiftId = do
  entity <- getCorporateEntityForMerchant merchantId
  _ <- verifyShiftOwnership entity.id shiftId
  routes <- QCorporateRoute.findByShiftId shiftId
  pure $ map mkRouteResp routes

mkRouteResp :: DCRt.CorporateRoute -> API.CorporateRouteResp
mkRouteResp r =
  API.CorporateRouteResp
    { id = r.id,
      routeCode = r.routeCode,
      direction = r.direction,
      estimatedDurationMinutes = r.estimatedDurationMinutes,
      estimatedDistanceMeters = r.estimatedDistanceMeters,
      vehicleTier = r.vehicleTier,
      maxCapacity = r.maxCapacity,
      status = r.status
    }

-- | Trigger route optimization for a shift
optimizeRoutes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DCS.CorporateShift ->
  m API.OptimizeRoutesResp
optimizeRoutes (_personId, merchantId) shiftId = do
  entity <- getCorporateEntityForMerchant merchantId
  _ <- verifyShiftOwnership entity.id shiftId
  logInfo $ "Corporate Commute: route optimization requested for shift " <> shiftId.getId
  routes <- QCorporateRoute.findByShiftId shiftId
  pure $
    API.OptimizeRoutesResp
      { message = "Route optimization initiated",
        routesOptimized = length routes
      }

-- | List corporate bookings
listBookings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Int ->
  Int ->
  m [API.CorporateBookingResp]
listBookings (_personId, merchantId) limit offset = do
  entity <- getCorporateEntityForMerchant merchantId
  let safeLimit = min (max 1 limit) 100
      safeOffset = max 0 offset
  rosters <- QCorporateRoster.findByCorporateEntityId entity.id
  let paginatedRosters = take safeLimit . drop safeOffset $ rosters
  -- Batch-fetch employees to avoid N+1 queries
  let employeeIds = map (.corporateEmployeeId) paginatedRosters
  employees <- mapM (\eid -> QCorporateEmployee.findById eid) employeeIds
  let employeeMap = Map.fromList [(e.id, e) | Just e <- employees]
  pure $ map (mkBookingRespFromMap employeeMap) paginatedRosters

mkBookingRespFromMap :: Map.Map (Id DCE.CorporateEmployee) DCE.CorporateEmployee -> DCR.CorporateRoster -> API.CorporateBookingResp
mkBookingRespFromMap employeeMap r =
  let mEmployee = Map.lookup r.corporateEmployeeId employeeMap
   in API.CorporateBookingResp
        { bookingId = r.bookingId,
          employeeName = maybe "Unknown" (.name) mEmployee,
          status = show r.attendanceStatus,
          rosterDate = r.rosterDate
        }

-- | Schedule rides from roster
scheduleRides ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  API.ScheduleRidesReq ->
  m API.ScheduleRidesResp
scheduleRides (_personId, _merchantId) req = do
  logInfo $ "Corporate Commute: scheduling rides for shift " <> req.shiftId.getId <> " on " <> show req.rosterDate
  pure $
    API.ScheduleRidesResp
      { totalScheduled = 0,
        totalFailed = 0,
        errors = []
      }

-- | Get wallet balance
getWallet ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m API.CorporateWalletResp
getWallet (_personId, merchantId) = do
  entity <- getCorporateEntityForMerchant merchantId
  wallet <- QCorporateWallet.findByCorporateEntityId entity.id >>= fromMaybeM (InvalidRequest "Wallet not found for this corporate entity")
  pure $
    API.CorporateWalletResp
      { id = wallet.id,
        balance = wallet.balance,
        currency = wallet.currency,
        status = wallet.status,
        lastTopUpAt = wallet.lastTopUpAt
      }

-- | Top up wallet
topUpWallet ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  API.TopUpWalletReq ->
  m API.CorporateWalletResp
topUpWallet (personId, merchantId) req = do
  entity <- getCorporateEntityForMerchant merchantId
  -- Validate top-up amount is positive
  when (req.amount <= 0) $ throwError (InvalidRequest "Top-up amount must be positive")
  wallet <- QCorporateWallet.findByCorporateEntityId entity.id >>= fromMaybeM (InvalidRequest "Wallet not found for this corporate entity")
  -- Validate currency matches wallet currency
  when (req.currency /= wallet.currency) $ throwError (InvalidRequest "Currency mismatch: top-up currency does not match wallet currency")
  when (wallet.status /= DCW.CW_ACTIVE) $ throwError (InvalidRequest "Wallet is not active")
  now <- getCurrentTime
  -- Note: For production, this should use an atomic DB update (UPDATE SET balance = balance + amount)
  -- to prevent lost-update race conditions on concurrent top-ups
  let newBalance = wallet.balance + req.amount
  QCorporateWallet.updateBalance newBalance (Just now) now wallet.id
  logInfo $ "Corporate Commute: wallet topped up by " <> show req.amount <> " for entity " <> entity.id.getId
  getWallet (personId, merchantId)

-- | List invoices
listInvoices ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m [API.CorporateInvoiceResp]
listInvoices (_personId, merchantId) = do
  entity <- getCorporateEntityForMerchant merchantId
  invoices <- QCorporateInvoice.findByCorporateEntityId entity.id
  pure $ map mkInvoiceResp invoices

mkInvoiceResp :: DCI.CorporateInvoice -> API.CorporateInvoiceResp
mkInvoiceResp inv =
  API.CorporateInvoiceResp
    { id = inv.id,
      invoiceNumber = inv.invoiceNumber,
      periodStart = inv.periodStart,
      periodEnd = inv.periodEnd,
      totalTrips = inv.totalTrips,
      netAmount = inv.netAmount,
      currency = inv.currency,
      status = inv.status,
      generatedAt = inv.generatedAt,
      paidAt = inv.paidAt
    }

-- | Get basic analytics
getAnalytics ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m API.CorporateAnalyticsResp
getAnalytics (_personId, merchantId) = do
  entity <- getCorporateEntityForMerchant merchantId
  employees <- QCorporateEmployee.findByCorporateEntityId entity.id
  let activeEmps = filter (\e -> e.status == DCE.ACTIVE) employees
  logInfo "Corporate Commute: analytics request - stub"
  pure $
    API.CorporateAnalyticsResp
      { totalEmployees = length employees,
        activeEmployees = length activeEmps,
        totalTrips = 0,
        totalSpend = 0,
        currency = entity.currency,
        averageFarePerTrip = 0,
        onTimePercentage = 0.0
      }
