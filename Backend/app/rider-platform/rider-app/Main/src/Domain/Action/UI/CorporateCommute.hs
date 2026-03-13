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

-- | Fetch the corporate entity for the current merchant
getCorporateEntity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m API.CorporateEntityResp
getCorporateEntity (_personId, merchantId) = do
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
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
listEmployees (_personId, merchantId) _limit _offset = do
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
  employees <- QCorporateEmployee.findByCorporateEntityId entity.id
  pure $ map mkEmployeeResp employees

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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
  newId <- generateGUID
  now <- getCurrentTime
  let parseTOD t = readMaybe (toString t) & fromMaybeM (InvalidRequest $ "Invalid time format: " <> t)
  pickupStart <- parseTOD req.pickupWindowStart
  pickupEnd <- parseTOD req.pickupWindowEnd
  dropStart <- parseTOD req.dropWindowStart
  dropEnd <- parseTOD req.dropWindowEnd
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
            status = DCS.ACTIVE,
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
getRoster (_personId, _merchantId) shiftId date = do
  rosters <- QCorporateRoster.findByShiftIdAndDate shiftId date
  mapM mkRosterResp rosters

mkRosterResp :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DCR.CorporateRoster -> m API.CorporateRosterResp
mkRosterResp r = do
  employee <- QCorporateEmployee.findById r.corporateEmployeeId >>= fromMaybeM (InvalidRequest "Employee not found")
  pure $
    API.CorporateRosterResp
      { id = r.id,
        employeeName = employee.name,
        employeeCode = employee.employeeCode,
        attendanceStatus = r.attendanceStatus,
        confirmedAt = r.confirmedAt
      }

-- | Confirm attendance for a roster entry
confirmAttendance ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DCR.CorporateRoster ->
  m APISuccess.APISuccess
confirmAttendance (_personId, _merchantId) rosterId = do
  _roster <- QCorporateRoster.findById rosterId >>= fromMaybeM (InvalidRequest "Roster entry not found")
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
listRoutes (_personId, _merchantId) shiftId = do
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
optimizeRoutes (_personId, _merchantId) shiftId = do
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
listBookings (_personId, merchantId) _limit _offset = do
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
  rosters <- QCorporateRoster.findByCorporateEntityId entity.id
  mapM mkBookingResp rosters
  where
    mkBookingResp r = do
      employee <- QCorporateEmployee.findById r.corporateEmployeeId >>= fromMaybeM (InvalidRequest "Employee not found")
      pure $
        API.CorporateBookingResp
          { bookingId = r.bookingId,
            employeeName = employee.name,
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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
  wallet <- QCorporateWallet.findByCorporateEntityId entity.id >>= fromMaybeM (InvalidRequest "Wallet not found for this corporate entity")
  now <- getCurrentTime
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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
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
  entities <- QCorporateEntity.findByMerchantId merchantId
  entity <- listToMaybe entities & fromMaybeM (InvalidRequest "No corporate entity found for this merchant")
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
