{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UnifiedDashboard where

import qualified API.Action.UnifiedDashboard.Provider as ProviderDSL
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

type APIV2 =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" Context.City
    :> ProviderDSLAPI

type ProviderDSLAPI = DashboardTokenAuth :> ProviderDSL.API

handlerV2 :: FlowServer APIV2
handlerV2 =
  \merchantId city -> unifiedDashboardProviderDSLHandler merchantId city

unifiedDashboardProviderDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer ProviderDSLAPI
unifiedDashboardProviderDSLHandler merchantId city _auth = ProviderDSL.handler merchantId city
