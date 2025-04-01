module Domain.Action.RiderPlatform.AppManagement.Tickets
  ( postTicketsVerify,
    postTicketsServices,
    getTicketsPlaces,
    postTicketsUpdate,
    postTicketsBookingsCancel,
    postTicketsServiceCancel,
    getTicketsBookingDetails,
    postTicketsTicketdashboardRegister,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Tickets
import qualified "rider-app" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified "lib-dashboard" Domain.Action.Dashboard.Person as DP
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person.Type as PT
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.ServerName as DTServer
import qualified "rider-app" Domain.Types.TicketBooking
import qualified "rider-app" Domain.Types.TicketBookingService
import qualified "rider-app" Domain.Types.TicketPlace
import qualified "rider-app" Domain.Types.TicketService
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.Transaction
import Storage.Beam.BeamFlow
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsVerify) personServiceId ticketBookingShortId)

postTicketsServices ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsServices) ticketPlaceId date

getTicketsPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace])
getTicketsPlaces merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsPlaces)

postTicketsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsUpdate) req

postTicketsBookingsCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketBookingCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsBookingsCancel merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsBookingsCancel) req

postTicketsServiceCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketServiceCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsServiceCancel merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsServiceCancel) req

getTicketsBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow API.Types.UI.TicketService.TicketBookingDetails)
getTicketsBookingDetails merchantShortId opCity apiTokenInfo ticketBookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsBookingDetails) ticketBookingShortId

postTicketsTicketdashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp)
postTicketsTicketdashboardRegister merchantShortId opCity req = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  res <- API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsTicketdashboardRegister) req
  registerTicketDashboard req res.id

------------------------------------ HELPER FUNCTIONS --------------------------------

validateTicketDashboardRegister :: Validate API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq
validateTicketDashboardRegister API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "lastName" lastName $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

buildTicketDashboardUser ::
  (EncFlow m r) =>
  API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq ->
  Maybe Text ->
  Kernel.Types.Id.Id DRole.Role ->
  DRole.DashboardAccessType ->
  m PT.Person
buildTicketDashboardUser req mbPersonId roleId dashboardAccessType = do
  pid <- case mbPersonId of
    Just personId -> return $ Kernel.Types.Id.Id personId
    Nothing -> generateGUID
  now <- getCurrentTime
  mobileNumber <- encrypt req.mobileNumber
  return
    PT.Person
      { id = pid,
        firstName = req.firstName,
        lastName = req.lastName,
        roleId = roleId,
        email = Nothing,
        mobileNumber = mobileNumber,
        mobileCountryCode = req.mobileCountryCode,
        passwordHash = Nothing,
        dashboardAccessType = Just dashboardAccessType,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        verified = Nothing
      }

registerTicketDashboard :: (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]) => API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Maybe Text -> m API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp
registerTicketDashboard req mbPersonId = do
  runRequestValidation validateTicketDashboardRegister req
  ticketDashboardRole <-
    QRole.findByDashboardAccessType DRole.TICKET_DASHBOARD_USER
      >>= fromMaybeM (RoleDoesNotExist "TICKET_DASHBOARD_USER")
  unlessM (isNothing <$> QP.findByMobileNumberAndRoleId req.mobileNumber req.mobileCountryCode ticketDashboardRole.id) $
    throwError (InvalidRequest "Phone already registered")
  ticketUser <- buildTicketDashboardUser req mbPersonId ticketDashboardRole.id ticketDashboardRole.dashboardAccessType
  merchant <-
    QMerchant.findByShortId (Kernel.Types.Id.ShortId req.merchantId)
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId)
  merchantServerAccessCheck merchant
  let city' = merchant.defaultOperatingCity
  merchantAccess <- DP.buildMerchantAccess ticketUser.id merchant.id merchant.shortId city'
  QP.create ticketUser
  QAccess.create merchantAccess
  return $ API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp {id = Nothing, success = True, message = Just "Ticket dashboard user registered successfully"}
