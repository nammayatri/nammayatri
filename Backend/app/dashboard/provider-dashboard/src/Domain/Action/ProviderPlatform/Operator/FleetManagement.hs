module Domain.Action.ProviderPlatform.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetUnlink,
    postFleetManagementFleetLinkSendOtp,
    postFleetManagementFleetLinkVerifyOtp,
    postFleetManagementFleetMemberAssociationCreate,
  )
where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Domain.Action.Dashboard.Registration as DRegistration
import qualified Domain.Action.ProviderPlatform.Fleet.RegistrationV2 as DRegistrationV2
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Domain.Types.Role
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

getFleetManagementFleets ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow API.Types.ProviderPlatform.Operator.FleetManagement.FleetInfoRes
getFleetManagementFleets merchantShortId opCity apiTokenInfo mbIsActive mbVerified mbEnabled mbLimit mbOffset mbSearchString = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.getFleetManagementFleets) mbIsActive mbVerified mbEnabled mbLimit mbOffset mbSearchString apiTokenInfo.personId.getId

postFleetManagementFleetRegisterClientCall :: DRegistrationV2.RegisterClientCall
postFleetManagementFleetRegisterClientCall checkedMerchantId opCity requestorId req' =
  Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetRegister) requestorId req'

postFleetManagementFleetRegister ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2 ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetRegister = DRegistrationV2.postRegistrationV2Register' postFleetManagementFleetRegisterClientCall

-- TODO remove duplication with postRegistrationV2LoginOtp
postFleetManagementFleetCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLoginReqV2 ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation API.Types.ProviderPlatform.Fleet.RegistrationV2.validateInitiateLoginReqV2 req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let merchant = apiTokenInfo.merchant
  let enabled = not $ fromMaybe False merchant.requireAdminApprovalForFleetOnboarding
  unless (opCity `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  merchantServerAccessCheck merchant
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  let req' = DRegistrationV2.buildFleetOwnerRegisterReqV2 merchantShortId opCity req
  fleetOwnerRole <- QRole.findByDashboardAccessType FLEET_OWNER >>= fromMaybeM (RoleNotFound $ show FLEET_OWNER)
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  res <-
    SharedLogic.Transaction.withResponseTransactionStoring transaction $
      Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetCreate) (Just enabled) apiTokenInfo.personId.getId req
  when (isNothing mbPerson) $ do
    let personId = Kernel.Types.Id.cast @API.Types.ProviderPlatform.Fleet.RegistrationV2.Person @DP.Person res.personId
    DRegistration.createFleetOwnerDashboardOnly fleetOwnerRole merchant req' personId
  pure Kernel.Types.APISuccess.Success

postFleetManagementFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetUnlink) fleetOwnerId apiTokenInfo.personId.getId

postFleetManagementFleetLinkSendOtp :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpReq -> Environment.Flow API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpRes
postFleetManagementFleetLinkSendOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  let createReq = API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLoginReqV2 req.mobileNumber req.mobileCountryCode
  let req' = DRegistrationV2.buildFleetOwnerRegisterReqV2 merchantShortId opCity createReq
  res <-
    SharedLogic.Transaction.withResponseTransactionStoring transaction $
      Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetLinkSendOtp) apiTokenInfo.personId.getId req
  when (isNothing mbPerson) $ do
    fleetOwnerRole <- QRole.findByDashboardAccessType FLEET_OWNER >>= fromMaybeM (RoleNotFound $ show FLEET_OWNER)
    let personId = Kernel.Types.Id.cast @API.Types.ProviderPlatform.Fleet.RegistrationV2.Person @DP.Person res.fleetOwnerId
    DRegistration.createFleetOwnerDashboardOnly fleetOwnerRole apiTokenInfo.merchant req' personId
  pure res

postFleetManagementFleetLinkVerifyOtp :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerVerifyOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetLinkVerifyOtp) apiTokenInfo.personId.getId req

postFleetManagementFleetMemberAssociationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetMemberAssociationCreateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetMemberAssociationCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetMemberAssociationCreate) req
