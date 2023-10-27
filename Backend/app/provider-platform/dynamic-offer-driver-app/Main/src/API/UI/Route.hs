{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Route
  ( API,
    handler,
    DRoute.GetRoutesReq,
    DRoute.GetRoutesResp,
  )
where

import qualified Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Maps.GetRoutesReq
    :> Post '[JSON] Maps.GetRoutesResp
    :<|> "pickup"
      :> "route"
      :> TokenAuth
      :> ReqBody '[JSON] Maps.GetRoutesReq
      :> Post '[JSON] Maps.GetRoutesResp
    :<|> "trip"
      :> "route"
      :> TokenAuth
      :> ReqBody '[JSON] Maps.GetRoutesReq
      :> Post '[JSON] Maps.GetRoutesResp

handler :: FlowServer API
handler = getRoute :<|> getPickupRoute :<|> getTripRoute

getRoute :: (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getRoute (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . withPersonIdLogTag personId . DRoute.getRoutes (personId, merchantId, merchantOpCityId)

getPickupRoute :: (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getPickupRoute (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . withPersonIdLogTag personId . DRoute.getPickupRoutes (personId, merchantId, merchantOpCityId)

getTripRoute :: (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getTripRoute (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . withPersonIdLogTag personId . DRoute.getTripRoutes (personId, merchantId, merchantOpCityId)
