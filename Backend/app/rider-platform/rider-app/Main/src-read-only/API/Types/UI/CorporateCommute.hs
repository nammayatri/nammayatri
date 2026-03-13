{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CorporateCommute where

import Data.OpenApi (ToSchema)
import Data.Time.Calendar (Day)
import qualified Domain.Types.Booking
import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateInvoice
import qualified Domain.Types.CorporateRoute
import qualified Domain.Types.CorporateRoster
import qualified Domain.Types.CorporateShift
import qualified Domain.Types.CorporateWallet
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data CorporateEntityResp = CorporateEntityResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    name :: Kernel.Prelude.Text,
    registeredName :: Kernel.Prelude.Text,
    gstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    industry :: Kernel.Prelude.Text,
    contactPersonName :: Kernel.Prelude.Text,
    contactEmail :: Kernel.Prelude.Text,
    contactPhone :: Kernel.Prelude.Text,
    billingModel :: Domain.Types.CorporateEntity.CorporateBillingModel,
    billingCycleType :: Domain.Types.CorporateEntity.BillingCycleType,
    creditLimit :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    status :: Domain.Types.CorporateEntity.CorporateEntityStatus,
    contractStartDate :: Kernel.Prelude.UTCTime,
    contractEndDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateEmployeeResp = CorporateEmployeeResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee,
    employeeCode :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    email :: Kernel.Prelude.Text,
    phone :: Kernel.Prelude.Text,
    department :: Kernel.Prelude.Text,
    costCenter :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.CorporateEmployee.CorporateEmployeeStatus,
    linkedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateEmployeeReq = CreateEmployeeReq
  { employeeCode :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    email :: Kernel.Prelude.Text,
    phone :: Kernel.Prelude.Text,
    department :: Kernel.Prelude.Text,
    costCenter :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    defaultPickupLat :: Kernel.Prelude.Double,
    defaultPickupLon :: Kernel.Prelude.Double,
    defaultPickupAddress :: Kernel.Prelude.Text,
    reportingManagerEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUploadReq = BulkUploadReq
  { csvData :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUploadResp = BulkUploadResp
  { totalRows :: Kernel.Prelude.Int,
    successCount :: Kernel.Prelude.Int,
    errorCount :: Kernel.Prelude.Int,
    errors :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateShiftResp = CorporateShiftResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    name :: Kernel.Prelude.Text,
    pickupWindowStart :: Kernel.Prelude.Text,
    pickupWindowEnd :: Kernel.Prelude.Text,
    dropWindowStart :: Kernel.Prelude.Text,
    dropWindowEnd :: Kernel.Prelude.Text,
    activeDays :: Kernel.Prelude.Text,
    isNightShift :: Kernel.Prelude.Bool,
    maxOccupancy :: Kernel.Prelude.Int,
    allowedVehicleTiers :: Kernel.Prelude.Text,
    confirmationDeadlineMinutes :: Kernel.Prelude.Int,
    status :: Domain.Types.CorporateShift.CorporateShiftStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateShiftReq = CreateShiftReq
  { name :: Kernel.Prelude.Text,
    pickupWindowStart :: Kernel.Prelude.Text,
    pickupWindowEnd :: Kernel.Prelude.Text,
    dropWindowStart :: Kernel.Prelude.Text,
    dropWindowEnd :: Kernel.Prelude.Text,
    activeDays :: Kernel.Prelude.Text,
    isNightShift :: Kernel.Prelude.Bool,
    maxOccupancy :: Kernel.Prelude.Int,
    allowedVehicleTiers :: Kernel.Prelude.Text,
    confirmationDeadlineMinutes :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateRosterResp = CorporateRosterResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster,
    employeeName :: Kernel.Prelude.Text,
    employeeCode :: Kernel.Prelude.Text,
    attendanceStatus :: Domain.Types.CorporateRoster.CorporateAttendanceStatus,
    confirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateRouteResp = CorporateRouteResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute,
    routeCode :: Kernel.Prelude.Text,
    direction :: Domain.Types.CorporateRoute.CorporateRouteDirection,
    estimatedDurationMinutes :: Kernel.Prelude.Int,
    estimatedDistanceMeters :: Kernel.Prelude.Int,
    vehicleTier :: Kernel.Prelude.Text,
    maxCapacity :: Kernel.Prelude.Int,
    status :: Domain.Types.CorporateRoute.CorporateRouteStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OptimizeRoutesResp = OptimizeRoutesResp
  { message :: Kernel.Prelude.Text,
    routesOptimized :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateBookingResp = CorporateBookingResp
  { bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Booking.Booking),
    employeeName :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    rosterDate :: Data.Time.Calendar.Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduleRidesReq = ScheduleRidesReq
  { shiftId :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    rosterDate :: Data.Time.Calendar.Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduleRidesResp = ScheduleRidesResp
  { totalScheduled :: Kernel.Prelude.Int,
    totalFailed :: Kernel.Prelude.Int,
    errors :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateWalletResp = CorporateWalletResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateWallet.CorporateWallet,
    balance :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    status :: Domain.Types.CorporateWallet.CorporateWalletStatus,
    lastTopUpAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TopUpWalletReq = TopUpWalletReq
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateInvoiceResp = CorporateInvoiceResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateInvoice.CorporateInvoice,
    invoiceNumber :: Kernel.Prelude.Text,
    periodStart :: Kernel.Prelude.UTCTime,
    periodEnd :: Kernel.Prelude.UTCTime,
    totalTrips :: Kernel.Prelude.Int,
    netAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    status :: Domain.Types.CorporateInvoice.CorporateInvoiceStatus,
    generatedAt :: Kernel.Prelude.UTCTime,
    paidAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateAnalyticsResp = CorporateAnalyticsResp
  { totalEmployees :: Kernel.Prelude.Int,
    activeEmployees :: Kernel.Prelude.Int,
    totalTrips :: Kernel.Prelude.Int,
    totalSpend :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    averageFarePerTrip :: Kernel.Types.Common.HighPrecMoney,
    onTimePercentage :: Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
