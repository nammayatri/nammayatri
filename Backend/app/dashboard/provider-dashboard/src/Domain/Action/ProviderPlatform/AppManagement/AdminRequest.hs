module Domain.Action.ProviderPlatform.AppManagement.AdminRequest
  ( postAdminRequestCreate,
    getAdminRequestList,
    postAdminRequestRespond,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.AdminRequest
import qualified Domain.Types.AdminRequest
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postAdminRequestCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.AdminRequest.CreateAdminRequestReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postAdminRequestCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
      requestorName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.adminRequestDSL.postAdminRequestCreate) requestorId requestorName req

getAdminRequestList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest) ->
  Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdminRequestStatus ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Domain.Types.AdminRequest.ActionType ->
  Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Domain.Types.AdminRequest.ReferenceTable ->
  Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow API.Types.Dashboard.AppManagement.AdminRequest.AdminRequestResp
getAdminRequestList merchantShortId opCity apiTokenInfo limit offset adminRequestId status personId excludeCurrentAdminMaker actionType adjustmentType referenceType referenceId referenceTable source adminMakerId adminCheckerId from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.adminRequestDSL.getAdminRequestList) limit offset adminRequestId status personId excludeCurrentAdminMaker actionType adjustmentType referenceType referenceId referenceTable source adminMakerId adminCheckerId from to requestorId

postAdminRequestRespond :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest -> API.Types.Dashboard.AppManagement.AdminRequest.RespondAdminRequestReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postAdminRequestRespond merchantShortId opCity apiTokenInfo adminRequestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
      requestorName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.adminRequestDSL.postAdminRequestRespond) adminRequestId requestorId requestorName req
