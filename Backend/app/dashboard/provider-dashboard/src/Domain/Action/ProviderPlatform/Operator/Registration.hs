module Domain.Action.ProviderPlatform.Operator.Registration (postOperatorRegister, postRegistrationDashboardRegister) where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.Registration as Common
import qualified "lib-dashboard" Domain.Action.Dashboard.Registration as DRegistration
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" SharedLogic.Transaction as ST
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Merchant

postOperatorRegister ::
  ShortId DM.Merchant ->
  Context.City ->
  ApiTokenInfo UserActionType ->
  Common.OperatorRegisterReq ->
  Flow APISuccess
postOperatorRegister merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  void $ merchantServerAccessCheck merchant
  unlessM (null <$> QP.findByEmailOrMobile req.email req.mobileNumber req.mobileCountryCode) $ throwError (InvalidRequest "Phone or Email already registered")
  transaction <- ST.buildTransaction (DT.ActionAPI apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  res <- ST.withTransactionStoring transaction do
    Client.callOperatorAPI checkedMerchantId opCity (.registrationDSL.postOperatorRegister) req
  DRegistration.registerOperatorDashboardOnly opCity req.email req.mobileNumber req.mobileCountryCode req.firstName req.lastName Nothing (cast res.personId) merchant Nothing
  pure Success

postRegistrationDashboardRegister :: (ShortId DM.Merchant -> Context.City -> ApiTokenInfo UserActionType -> Common.CreateDashboardOperatorReq -> Flow APISuccess)
postRegistrationDashboardRegister merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unlessM (null <$> QP.findByEmailOrMobile (Just req.email) req.mobileNumber req.mobileCountryCode) $ throwError (InvalidRequest "Phone or Email already registered")
  void $ merchantServerAccessCheck merchant
  res <- Client.callOperatorAPI checkedMerchantId opCity (.registrationDSL.postRegistrationDashboardRegister) req
  DRegistration.registerOperatorDashboardOnly opCity (Just req.email) req.mobileNumber req.mobileCountryCode req.firstName req.lastName (Just req.password) (cast res.personId) merchant (Just req.roleId)
  pure Success
