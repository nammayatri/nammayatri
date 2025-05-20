module Domain.Action.ProviderPlatform.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetUnlink,
    postFleetManagementFleetLinkSendOtp,
    postFleetManagementFleetLinkVerifyOtp,
  )
where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Domain.Action.Dashboard.Registration as DRegistration
import qualified "lib-dashboard" Domain.Types.Merchant
import "lib-dashboard" Domain.Types.Role
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
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
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow API.Types.ProviderPlatform.Operator.FleetManagement.FleetInfoRes
getFleetManagementFleets merchantShortId opCity apiTokenInfo mbIsActive mbVerified mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.getFleetManagementFleets) mbIsActive mbVerified mbLimit mbOffset apiTokenInfo.personId.getId

postFleetManagementFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerRegisterReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetRegister merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetRegister) apiTokenInfo.personId.getId req

postFleetManagementFleetCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerCreateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  let req' = buildFleetOwnerRegisterReq merchantShortId opCity req
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  res <-
    SharedLogic.Transaction.withResponseTransactionStoring transaction $
      Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetCreate) apiTokenInfo.personId.getId req
  when (isNothing mbPerson) $ do
    fleetOwnerRole <- QRole.findByDashboardAccessType FLEET_OWNER >>= fromMaybeM (RoleNotFound "FLEET_OWNER")
    DRegistration.createFleetOwnerDashboardOnly fleetOwnerRole apiTokenInfo.merchant req' (Just res.personId.getId) True
  pure Kernel.Types.APISuccess.Success

buildFleetOwnerRegisterReq ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerCreateReq ->
  DRegistration.FleetRegisterReq
buildFleetOwnerRegisterReq merchantShortId opCity API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerCreateReq {..} = do
  DRegistration.FleetRegisterReq
    { firstName = "FLEET",
      lastName = "OWNER", -- update in register
      merchantId = merchantShortId,
      fleetType = Nothing,
      city = Just opCity,
      email = Nothing,
      ..
    }

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
  let createReq = API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerCreateReq req.mobileNumber req.mobileCountryCode
  let req' = buildFleetOwnerRegisterReq merchantShortId opCity createReq
  res <-
    SharedLogic.Transaction.withResponseTransactionStoring transaction $
      Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetLinkSendOtp) apiTokenInfo.personId.getId req
  when (isNothing mbPerson) $ do
    fleetOwnerRole <- QRole.findByDashboardAccessType FLEET_OWNER >>= fromMaybeM (RoleNotFound "FLEET_OWNER")
    DRegistration.createFleetOwnerDashboardOnly fleetOwnerRole apiTokenInfo.merchant req' (Just res.fleetOwnerId.getId) True
  pure res

postFleetManagementFleetLinkVerifyOtp :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerVerifyOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetLinkVerifyOtp) apiTokenInfo.personId.getId req
