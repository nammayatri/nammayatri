{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Overlay
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Overlay as DOverlay
import Dashboard.Common as Common
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Overlay as DTOverlay
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "overlay"
    :> ( CreateOverlayAPI
           :<|> DeleteOverlayAPI
           :<|> ListOverlayAPI
           :<|> OverlayInfoAPI
           :<|> ScheduleOverlayAPI
       )

type CreateOverlayAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'OVERLAY 'CREATE_OVERLAY
    :> DOverlay.CreateOverlayAPI

type DeleteOverlayAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'OVERLAY 'DELETE_OVERLAY
    :> DOverlay.DeleteOverlayAPI

type ListOverlayAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'OVERLAY 'LIST_OVERLAY
    :> DOverlay.ListOverlayAPI

type OverlayInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'OVERLAY 'OVERLAY_INFO
    :> DOverlay.OverlayInfoAPI

type ScheduleOverlayAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'OVERLAY 'SCHEDULE_OVERLAY
    :> DOverlay.ScheduleOverlayAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  createOverlay merchantId city
    :<|> deleteOverlay merchantId city
    :<|> listOverlay merchantId city
    :<|> overlayInfo merchantId city
    :<|> scheduleOverlay merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  DOverlay.OverlayEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.OverlayAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

createOverlay :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> DTOverlay.CreateOverlayReq -> FlowHandler APISuccess
createOverlay merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction DOverlay.CreateOverlayEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.overlay.createOverlay) req

deleteOverlay :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> DTOverlay.DeleteOverlayReq -> FlowHandler APISuccess
deleteOverlay merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction DOverlay.DeleteOverlayEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.overlay.deleteOverlay) req

listOverlay :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler DTOverlay.ListOverlayResp
listOverlay merchantShortId opCity apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.overlay.listOverlay)

overlayInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> DTOverlay.OverlayInfoReq -> FlowHandler DTOverlay.OverlayInfoResp
overlayInfo merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.overlay.overlayInfo) req

scheduleOverlay :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> DTOverlay.ScheduleOverlay -> FlowHandler APISuccess
scheduleOverlay merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction DOverlay.ScheduleOverlayEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.overlay.scheduleOverlay) req
