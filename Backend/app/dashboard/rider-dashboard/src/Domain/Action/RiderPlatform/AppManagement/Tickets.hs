module Domain.Action.RiderPlatform.AppManagement.Tickets
  ( postTicketsVerify,
    postTicketsServices,
    getTicketsPlaces,
    getTicketFleetVehicles,
    postTicketsUpdate,
    postTicketsBookingsCancel,
    postTicketsServiceCancel,
    getTicketsBookingDetails,
    postTicketsTicketdashboardRegister,
    postTicketsTicketdashboardLoginAuth,
    postTicketsTicketdashboardLoginVerify,
    getTicketsTicketdashboardAgreement,
    getTicketsTicketdashboardUserInfo,
    getTicketsTicketdashboardFile,
    postTicketsTicketdashboardSendverifyotp,
    getTicketsTicketdashboardTicketplaceInfo,
    postTicketsTicketdashboardTicketplaceUpdate,
    getTicketsTicketdashboardTicketplaces,
    getTicketsTicketdashboardTicketplaceSubPlaces,
    postTicketsTicketdashboardTicketplaceUpdateSubPlaces,
    postTicketBookingsVerifyV2,
    postTicketPlacesBook,
    getTicketPlaces,
    getTicketPlaceServices,
    getTicketBookingDetails,
    getAllTicketBookings,
    postTicketBookingCashCollect,
    postTicketPlacesDirectBook,
    getTicketsDashboardBookingStatus,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Tickets
import qualified "rider-app" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified "lib-dashboard" Domain.Action.Dashboard.Person as DP
import "lib-dashboard" Domain.Action.Dashboard.Registration as DR
import Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding (getDashboardAccessType)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "rider-app" Domain.Types.MerchantOnboarding as DMO
import qualified "lib-dashboard" Domain.Types.Person.Type as PT
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.ServerName as DTServer
import qualified "rider-app" Domain.Types.TicketBooking
import qualified "rider-app" Domain.Types.TicketBookingService
import qualified "rider-app" Domain.Types.TicketDashboard
import qualified "rider-app" Domain.Types.TicketPlace
import qualified "rider-app" Domain.Types.TicketService
import qualified "rider-app" Domain.Types.TicketSubPlace
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
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
import "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Maybe Text -> Maybe Text -> Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId mbFleetOwnerId mbVehicleNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsVerify) personServiceId ticketBookingShortId mbFleetOwnerId mbVehicleNumber)

postTicketsServices ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) ->
  Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date subPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsServices) ticketPlaceId date subPlaceId

getTicketsPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace])
getTicketsPlaces merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsPlaces)

getTicketFleetVehicles ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Environment.Flow [API.Types.UI.TicketService.TicketFleetVehicleResp]
getTicketFleetVehicles merchantShortId opCity apiTokenInfo placeId mbLimit mbOffset mbSearchString = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketFleetVehicles) placeId mbLimit mbOffset mbSearchString

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
  ticketDashboardRoleIds <-
    ((.id) <$>) <$> QRole.findAllInDashboardAccessType [DRole.TICKET_DASHBOARD_USER, DRole.TICKET_DASHBOARD_MERCHANT, DRole.TICKET_DASHBOARD_ADMIN, DRole.TICKET_DASHBOARD_APPROVER]
  when (null ticketDashboardRoleIds) $ throwError $ InternalError "No ticket dashboard roles found"
  unlessM (isNothing <$> QP.findByMobileNumberAndRoleIdsWithType @'PT.TicketDashboard req.mobileNumber req.mobileCountryCode ticketDashboardRoleIds) $
    throwError (InvalidRequest "Phone already registered")
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
        verified = Nothing,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        dashboardType = PT.TICKET_DASHBOARD,
        passwordUpdatedAt = Nothing,
        approvedBy = Nothing,
        rejectedBy = Nothing
      }

registerTicketDashboard :: (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]) => API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Maybe Text -> m API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp
registerTicketDashboard req mbPersonId = do
  runRequestValidation validateTicketDashboardRegister req
  ticketDashboardRole <-
    QRole.findByDashboardAccessType DRole.TICKET_DASHBOARD_USER
      >>= fromMaybeM (RoleDoesNotExist "TICKET_DASHBOARD_USER")
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

postTicketsTicketdashboardLoginAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardLoginAuth merchantShortId opCity req = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsTicketdashboardLoginAuth) req

postTicketsTicketdashboardLoginVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginResp)
postTicketsTicketdashboardLoginVerify merchantShortId opCity req = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  ticketDashboardRoleIds <-
    ((.id) <$>) <$> QRole.findAllInDashboardAccessType [DRole.TICKET_DASHBOARD_USER, DRole.TICKET_DASHBOARD_MERCHANT, DRole.TICKET_DASHBOARD_ADMIN, DRole.TICKET_DASHBOARD_APPROVER]
  when (null ticketDashboardRoleIds) $ throwError $ InternalError "No ticket dashboard roles found"
  person <- QP.findByMobileNumberAndRoleIdsWithType @'PT.TicketDashboard req.mobileNumber req.mobileCountryCode ticketDashboardRoleIds >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _ <- API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsTicketdashboardLoginVerify) req
  token <- DR.generateToken person.id merchant merchant.defaultOperatingCity
  unless (person.verified == Just True) $ QP.updatePersonVerifiedStatus person.id True
  pure $ API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginResp {authToken = Just token}

getTicketsTicketdashboardAgreement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardAgreementTemplateResp)
getTicketsTicketdashboardAgreement merchantShortId opCity apiTokenInfo templateName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsTicketdashboardAgreement) templateName

getTicketsTicketdashboardUserInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardUserInfo)
getTicketsTicketdashboardUserInfo merchantShortId opCity apiTokenInfo requestorId' _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  let userId = fromMaybe apiTokenInfo.personId.getId requestorId'
  userRole <- getDashboardAccessType userId
  requestorRole <- getDashboardAccessType requestorId
  unless (userId == requestorId || requestorRole `elem` [DMO.TICKET_DASHBOARD_ADMIN]) $
    throwError $ InternalError "Operation not permitted"
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsTicketdashboardUserInfo) (pure userId) (pure userRole) (pure requestorRole)

getTicketsTicketdashboardFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.MerchantOnboarding.GetFileResponse)
getTicketsTicketdashboardFile merchantShortId opCity apiTokenInfo fileId requestorId requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsTicketdashboardFile) fileId requestorId requestorRole

postTicketsTicketdashboardSendverifyotp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.SendVerifyOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardSendverifyotp merchantShortId opCity req = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsTicketdashboardSendverifyotp) req

getTicketsTicketdashboardTicketplaceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.TicketDashboard.TicketPlaceDashboardDetails)
getTicketsTicketdashboardTicketplaceInfo merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId' _requestorRole' = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  --let requestorId = apiTokenInfo.personId.getId
  -- requestorRole <- getDashboardAccessType requestorId
  -- unless (requestorRole `elem` [DMO.TICKET_DASHBOARD_MERCHANT, DMO.TICKET_DASHBOARD_ADMIN]) $
  --   throwError $ InternalError "Operation not permitted"
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsTicketdashboardTicketplaceInfo) ticketPlaceId Nothing Nothing

postTicketsTicketdashboardTicketplaceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.TicketDashboard.TicketPlaceDashboardDetails -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdate merchantShortId opCity apiTokenInfo _requestorId' _requestorRole' req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  --let requestorId = apiTokenInfo.personId.getId
  -- requestorRole <- getDashboardAccessType requestorId
  -- unless (requestorRole `elem` [DMO.TICKET_DASHBOARD_MERCHANT, DMO.TICKET_DASHBOARD_ADMIN]) $
  --   throwError $ InternalError "Operation not permitted"
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsTicketdashboardTicketplaceUpdate) Nothing Nothing req

getTicketsTicketdashboardTicketplaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace])
getTicketsTicketdashboardTicketplaces merchantShortId opCity apiTokenInfo status _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  --let requestorId = apiTokenInfo.personId.getId
  -- requestorRole <- getDashboardAccessType requestorId
  -- unless (requestorRole `elem` [DMO.TICKET_DASHBOARD_MERCHANT, DMO.TICKET_DASHBOARD_ADMIN]) $
  --   throwError $ InternalError "Operation not permitted"
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsTicketdashboardTicketplaces) status Nothing Nothing

getTicketsTicketdashboardTicketplaceSubPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow [Domain.Types.TicketSubPlace.TicketSubPlace])
getTicketsTicketdashboardTicketplaceSubPlaces merchantShortId opCity apiTokenInfo ticketPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsTicketdashboardTicketplaceSubPlaces) ticketPlaceId

postTicketsTicketdashboardTicketplaceUpdateSubPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> [Domain.Types.TicketSubPlace.TicketSubPlace] -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdateSubPlaces merchantShortId opCity apiTokenInfo ticketPlaceId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsTicketdashboardTicketplaceUpdateSubPlaces) ticketPlaceId req

postTicketBookingsVerifyV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> API.Types.UI.TicketService.TicketServiceVerificationReq -> Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketBookingsVerifyV2 merchantShortId opCity apiTokenInfo personServiceId ticketServiceShortId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketBookingsVerifyV2) personServiceId ticketServiceShortId req

postTicketPlacesBook :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.UI.TicketService.TicketBookingReq -> Environment.Flow Kernel.External.Payment.Interface.Types.CreateOrderResp)
postTicketPlacesBook merchantShortId opCity apiTokenInfo placeId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketPlacesBook) placeId req

getTicketPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketPlaces)

getTicketPlaceServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> Environment.Flow [API.Types.UI.TicketService.TicketServiceResp])
getTicketPlaceServices merchantShortId opCity apiTokenInfo placeId date subPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketPlaceServices) placeId date subPlaceId

getTicketBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow API.Types.UI.TicketService.TicketBookingDetails)
getTicketBookingDetails merchantShortId opCity apiTokenInfo bookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketBookingDetails) bookingShortId

getTicketsDashboardBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Types.TicketBooking.BookingStatus)
getTicketsDashboardBookingStatus merchantShortId opCity apiTokenInfo userPhoneNumber bookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsDashboardBookingStatus) userPhoneNumber bookingShortId

getAllTicketBookings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.TicketBooking.BookingStatus) -> Environment.Flow [API.Types.UI.TicketService.TicketBookingAPIEntityV2])
getAllTicketBookings merchantShortId opCity apiTokenInfo limit offset status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getAllTicketBookings) limit offset status

postTicketBookingCashCollect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketBookingCashCollect merchantShortId opCity apiTokenInfo bookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketBookingCashCollect) bookingShortId

postTicketPlacesDirectBook :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Maybe Text -> API.Types.UI.TicketService.DirectTicketBookingReq -> Environment.Flow API.Types.UI.TicketService.DirectTicketBookingResp)
postTicketPlacesDirectBook merchantShortId opCity apiTokenInfo placeId _requestorId req = do
  let requestorId = apiTokenInfo.personId.getId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketPlacesDirectBook) placeId (Just requestorId) req
