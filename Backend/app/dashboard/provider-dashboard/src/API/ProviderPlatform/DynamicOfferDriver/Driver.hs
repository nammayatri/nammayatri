{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module API.ProviderPlatform.DynamicOfferDriver.Driver
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Fleet.Operations as Fleet
import qualified "dynamic-offer-driver-app" API.Dashboard.Management.Subscription as ADSubscription
import qualified "dashboard-helper-api" Dashboard.Common.Driver as Common
import qualified Data.Time as DT
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.Subscription as DSubscription
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Ride as DARide
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as DRide
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI')
import qualified ProviderPlatformClient.DynamicOfferDriver.Fleet as Client
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "driver"
    :> ( ListDriverRidesForFleetAPI
           :<|> DriverSubscriptionDriverFeeAndInvoiceUpdateAPI
           :<|> SendMessageToDriverViaDashboardAPI
       )

type ListDriverRidesForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'LIST_DRIVER_RIDES
    :> Fleet.ListDriverRidesForFleetAPI

type DriverSubscriptionDriverFeeAndInvoiceUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE
    :> Common.UpdateSubscriptionDriverFeeAndInvoiceAPI

type SendMessageToDriverViaDashboardAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'SEND_DASHBOARD_MESSAGE
    :> ADSubscription.SendMessageToDriverViaDashboardAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  listDriverRidesForFleet merchantId city
    :<|> updateSubscriptionDriverFeeAndInvoice merchantId city
    :<|> sendMessageToDriverViaDashboard merchantId city

buildManagementServerTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.DriverEndpoint ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Maybe request ->
  m DT.Transaction
buildManagementServerTransaction endpoint apiTokenInfo driverId =
  T.buildTransaction (DT.DriverAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) (Just driverId) Nothing

listDriverRidesForFleet :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe DRide.RideStatus -> Maybe DT.Day -> Maybe Text -> Maybe Int -> FlowHandler DARide.DriverRideListRes
listDriverRidesForFleet merchantShortId opCity apiTokenInfo driverId mbLimit mbOffset mbOnlyActive mbStatus mbDate mbFleetOwnerId mbNumOfDays =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.listDriverRidesForFleet) driverId mbLimit mbOffset mbOnlyActive mbStatus mbDate mbFleetOwnerId mbNumOfDays

updateSubscriptionDriverFeeAndInvoice ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Common.ServiceNames ->
  Common.SubscriptionDriverFeesAndInvoicesToUpdate ->
  FlowHandler Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateSubscriptionDriverFeeAndInvoice merchantShortId opCity apiTokenInfo driverId serviceName req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UpdateSubscriptionDriverFeeAndInvoiceEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.updateSubscriptionDriverFeeAndInvoice) driverId serviceName req

sendMessageToDriverViaDashboard :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> DSubscription.SendSmsReq -> FlowHandler APISuccess
sendMessageToDriverViaDashboard merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.SendMessageToDriverViaDashboardEndPoint apiTokenInfo driverId (Just $ DSubscription.VolunteerTransactionStorageReq apiTokenInfo.personId.getId driverId.getId (show req.messageKey) (show req.channel) (show $ fromMaybe "" req.overlayKey) (show $ fromMaybe "" req.messageId))
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.sendMessageToDriverViaDashboard) driverId apiTokenInfo.personId.getId req
