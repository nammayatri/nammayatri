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

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  createOverlay merchantId
    :<|> deleteOverlay merchantId
    :<|> listOverlay merchantId
    :<|> overlayInfo merchantId
    :<|> scheduleOverlay merchantId

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

createOverlay :: ShortId DM.Merchant -> ApiTokenInfo -> DTOverlay.CreateOverlayReq -> FlowHandler APISuccess
createOverlay merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction DOverlay.CreateOverlayEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.overlay.createOverlay) req

deleteOverlay :: ShortId DM.Merchant -> ApiTokenInfo -> DTOverlay.DeleteOverlayReq -> FlowHandler APISuccess
deleteOverlay merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction DOverlay.DeleteOverlayEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.overlay.deleteOverlay) req

listOverlay :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler DTOverlay.ListOverlayResp
listOverlay merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.overlay.listOverlay)

overlayInfo :: ShortId DM.Merchant -> ApiTokenInfo -> DTOverlay.OverlayInfoReq -> FlowHandler DTOverlay.OverlayInfoResp
overlayInfo merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.overlay.overlayInfo) req

scheduleOverlay :: ShortId DM.Merchant -> ApiTokenInfo -> DTOverlay.ScheduleOverlay -> FlowHandler APISuccess
scheduleOverlay merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction DOverlay.ScheduleOverlayEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.overlay.scheduleOverlay) req
