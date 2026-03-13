{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CorporateCommute
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CorporateCommute
import qualified Control.Lens
import Data.Time.Calendar (Day)
import qualified Domain.Action.UI.CorporateCommute
import qualified Domain.Types.CorporateRoster
import qualified Domain.Types.CorporateShift
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "corporate" :> "entity" :> Get '[JSON] API.Types.UI.CorporateCommute.CorporateEntityResp
      :<|> TokenAuth :> "corporate" :> "employees" :> MandatoryQueryParam "limit" Int :> MandatoryQueryParam "offset" Int :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateEmployeeResp]
      :<|> TokenAuth :> "corporate" :> "employees" :> ReqBody '[JSON] API.Types.UI.CorporateCommute.CreateEmployeeReq :> Post '[JSON] API.Types.UI.CorporateCommute.CorporateEmployeeResp
      :<|> TokenAuth :> "corporate" :> "employees" :> "bulkUpload" :> ReqBody '[JSON] API.Types.UI.CorporateCommute.BulkUploadReq :> Post '[JSON] API.Types.UI.CorporateCommute.BulkUploadResp
      :<|> TokenAuth :> "corporate" :> "shifts" :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateShiftResp]
      :<|> TokenAuth :> "corporate" :> "shifts" :> ReqBody '[JSON] API.Types.UI.CorporateCommute.CreateShiftReq :> Post '[JSON] API.Types.UI.CorporateCommute.CorporateShiftResp
      :<|> TokenAuth :> "corporate" :> "roster" :> Capture "shiftId" (Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift) :> Capture "date" Day :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateRosterResp]
      :<|> TokenAuth :> "corporate" :> "roster" :> Capture "rosterId" (Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster) :> "confirm" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth :> "corporate" :> "routes" :> Capture "shiftId" (Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift) :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateRouteResp]
      :<|> TokenAuth :> "corporate" :> "routes" :> Capture "shiftId" (Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift) :> "optimize" :> Post '[JSON] API.Types.UI.CorporateCommute.OptimizeRoutesResp
      :<|> TokenAuth :> "corporate" :> "bookings" :> MandatoryQueryParam "limit" Int :> MandatoryQueryParam "offset" Int :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateBookingResp]
      :<|> TokenAuth :> "corporate" :> "bookings" :> "schedule" :> ReqBody '[JSON] API.Types.UI.CorporateCommute.ScheduleRidesReq :> Post '[JSON] API.Types.UI.CorporateCommute.ScheduleRidesResp
      :<|> TokenAuth :> "corporate" :> "wallet" :> Get '[JSON] API.Types.UI.CorporateCommute.CorporateWalletResp
      :<|> TokenAuth :> "corporate" :> "wallet" :> "topup" :> ReqBody '[JSON] API.Types.UI.CorporateCommute.TopUpWalletReq :> Post '[JSON] API.Types.UI.CorporateCommute.CorporateWalletResp
      :<|> TokenAuth :> "corporate" :> "invoices" :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateInvoiceResp]
      :<|> TokenAuth :> "corporate" :> "analytics" :> Get '[JSON] API.Types.UI.CorporateCommute.CorporateAnalyticsResp
  )

handler :: Environment.FlowServer API
handler =
  getCorporateEntity
    :<|> getCorporateEmployees
    :<|> postCorporateEmployees
    :<|> postCorporateEmployeesBulkUpload
    :<|> getCorporateShifts
    :<|> postCorporateShifts
    :<|> getCorporateRoster
    :<|> postCorporateRosterConfirm
    :<|> getCorporateRoutes
    :<|> postCorporateRoutesOptimize
    :<|> getCorporateBookings
    :<|> postCorporateBookingsSchedule
    :<|> getCorporateWallet
    :<|> postCorporateWalletTopup
    :<|> getCorporateInvoices
    :<|> getCorporateAnalytics

getCorporateEntity ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateEntityResp
  )
getCorporateEntity a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getCorporateEntity a1

getCorporateEmployees ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Int ->
    Int ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateEmployeeResp]
  )
getCorporateEmployees a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.listEmployees a3 a2 a1

postCorporateEmployees ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CorporateCommute.CreateEmployeeReq ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateEmployeeResp
  )
postCorporateEmployees a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.createEmployee a2 a1

postCorporateEmployeesBulkUpload ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CorporateCommute.BulkUploadReq ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.BulkUploadResp
  )
postCorporateEmployeesBulkUpload a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.bulkUploadEmployees a2 a1

getCorporateShifts ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateShiftResp]
  )
getCorporateShifts a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.listShifts a1

postCorporateShifts ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CorporateCommute.CreateShiftReq ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateShiftResp
  )
postCorporateShifts a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.createShift a2 a1

getCorporateRoster ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
    Day ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateRosterResp]
  )
getCorporateRoster a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getRoster a3 a2 a1

postCorporateRosterConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postCorporateRosterConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.confirmAttendance a2 a1

getCorporateRoutes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateRouteResp]
  )
getCorporateRoutes a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.listRoutes a2 a1

postCorporateRoutesOptimize ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.OptimizeRoutesResp
  )
postCorporateRoutesOptimize a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.optimizeRoutes a2 a1

getCorporateBookings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Int ->
    Int ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateBookingResp]
  )
getCorporateBookings a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.listBookings a3 a2 a1

postCorporateBookingsSchedule ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CorporateCommute.ScheduleRidesReq ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.ScheduleRidesResp
  )
postCorporateBookingsSchedule a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.scheduleRides a2 a1

getCorporateWallet ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateWalletResp
  )
getCorporateWallet a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getWallet a1

postCorporateWalletTopup ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CorporateCommute.TopUpWalletReq ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateWalletResp
  )
postCorporateWalletTopup a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.topUpWallet a2 a1

getCorporateInvoices ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateInvoiceResp]
  )
getCorporateInvoices a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.listInvoices a1

getCorporateAnalytics ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateAnalyticsResp
  )
getCorporateAnalytics a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getAnalytics a1
