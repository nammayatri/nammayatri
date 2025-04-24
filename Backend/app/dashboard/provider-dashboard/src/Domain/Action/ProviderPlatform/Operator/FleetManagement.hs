module Domain.Action.ProviderPlatform.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetLink,
    postFleetManagementFleetUnlink,
  )
where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Domain.Action.Dashboard.Registration as DRegistration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant

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
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  res <-
    SharedLogic.Transaction.withResponseTransactionStoring transaction $
      Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetCreate) apiTokenInfo.personId.getId req
  case mbPerson of
    Just _ -> pure Kernel.Types.APISuccess.Success
    Nothing -> do
      let req' = buildFleetOwnerRegisterReq merchantShortId opCity req
      void $ DRegistration.registerFleetOwner True req' $ Just res.personId.getId
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

postFleetManagementFleetLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetLink merchantShortId opCity apiTokenInfo fleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetLink) fleetOwnerId apiTokenInfo.personId.getId

postFleetManagementFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    Client.callOperatorAPI checkedMerchantId opCity (.fleetManagementDSL.postFleetManagementFleetUnlink) fleetOwnerId apiTokenInfo.personId.getId
