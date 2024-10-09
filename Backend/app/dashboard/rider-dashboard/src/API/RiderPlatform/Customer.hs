{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Customer
  ( API,
    handler,
  )
where

import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "customer"
    :> ( ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'CUSTOMER_CANCELLATION_DUES_DETAILS
           :> Common.GetCancellationDuesDetailsAPI
           :<|> ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'UPDATE_SAFETY_CENTER
             :> Common.UpdateSafetyCenterBlockingAPI
           :<|> ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'PERSON_NUMBERS
             :> Common.PostCustomersPersonNumbersAPI
           :<|> ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'FETCH_PERSON_ID
             :> Common.PostDriverPersonIdAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  getCancellationDuesDetails merchantId city
    :<|> updateSafetyCenterBlocking merchantId city
    :<|> postCustomersPersonNumbers merchantId city
    :<|> postCustomersPersonId merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.CustomerEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.CustomerAPI endpoint) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

getCancellationDuesDetails :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Customer -> FlowHandler Common.CancellationDuesDetailsRes
getCancellationDuesDetails merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.customers.getCancellationDuesDetails) customerId

updateSafetyCenterBlocking :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Customer -> Common.UpdateSafetyCenterBlockingReq -> FlowHandler APISuccess
updateSafetyCenterBlocking merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpdateSafetyCenterEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.customers.updateSafetyCenterBlocking) customerId req

postCustomersPersonNumbers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.PersonIdsReq -> FlowHandler [Common.PersonRes]
postCustomersPersonNumbers merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostCustomerPersonNumbersEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.customers.postCustomersPersonNumbers)) req

postCustomersPersonId :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.PersonMobileNoReq -> FlowHandler [Common.PersonRes]
postCustomersPersonId merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostCustomerPersonNumbersEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.customers.postCustomersPersonId)) req
