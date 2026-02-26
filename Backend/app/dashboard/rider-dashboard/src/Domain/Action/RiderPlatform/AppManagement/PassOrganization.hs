{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.AppManagement.PassOrganization
  ( getPassOrganizationGetPassOrganizationId,
    getPassOrganizationPassDetails,
    postPassOrganizationPassDetailsVerify,
    postPassOrganizationUpdate,
    getPassOrganizationGetOrganizations
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.PassOrganization
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.PassDetails
import qualified "rider-app" Domain.Types.PassOrganization
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.PassType
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
import Tools.Auth.Api
import Tools.Auth.Merchant

getPassOrganizationGetPassOrganizationId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization))
getPassOrganizationGetPassOrganizationId merchantShortId opCity apiTokenInfo personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.passOrganizationDSL.getPassOrganizationGetPassOrganizationId) personId

getPassOrganizationPassDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> Kernel.Prelude.Maybe (Domain.Types.PassDetails.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetails merchantShortId opCity apiTokenInfo passOrganizationId status limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.passOrganizationDSL.getPassOrganizationPassDetails) passOrganizationId status limit offset

postPassOrganizationPassDetailsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.PassOrganization.VerifyPassDetailsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.passOrganizationDSL.postPassOrganizationPassDetailsVerify) req)

postPassOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassOrganizationUpdate merchantShortId opCity apiTokenInfo personId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.passOrganizationDSL.postPassOrganizationUpdate) personId req)

getPassOrganizationGetOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.PassType.PassEnum -> Environment.Flow [API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp])
getPassOrganizationGetOrganizations merchantShortId opCity apiTokenInfo passEnum = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.passOrganizationDSL.getPassOrganizationGetOrganizations) passEnum