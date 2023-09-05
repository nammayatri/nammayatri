{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.RegistryMap where

import qualified "dynamic-offer-driver-app" API.Dashboard.RegistryMap as RD
import Domain.Types.AccessMatrix
import "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "dynamic-offer-driver-app" Domain.Types.RegistryMapFallback as DRM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  ApiAuth 'DRIVER_OFFER_BPP 'ADMIN 'UPDATE_REGISTRY_MAP
    :> RD.API

buildTransaction ::
  ( MonadFlow m
  ) =>
  RD.UpdateRegistryMapEndpoint ->
  ApiTokenInfo ->
  DRM.RegistryMapReq ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo regMapReq =
  T.buildTransaction (DT.UpdateRegistryMapAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing (Just regMapReq)

handler :: ShortId DMerchant.Merchant -> FlowServer API
handler = updateRegistryMap

updateRegistryMap :: ShortId DMerchant.Merchant -> ApiTokenInfo -> DRM.RegistryMapReq -> FlowHandler APISuccess
updateRegistryMap merchantShortId apiTokenInfo regMapReq = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction RD.RegistryMapEndpoint apiTokenInfo regMapReq
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.registry.updateRegistry) regMapReq
