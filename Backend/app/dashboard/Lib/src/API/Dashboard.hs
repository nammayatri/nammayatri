{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | provider-dashboard's login and administration tree.
--
-- Most of it now lives in lib-dashboard as 'API.DashboardLogin', so an
-- application server can serve the same routes directly. What remains here are
-- the routes whose types name @Domain.Types.Transaction.Endpoint@ -- a sum over
-- the API action types of both
-- application packages, promoted to the type level by the generated proxy tree,
-- and therefore not linkable by an application server.
--
-- Mounting both halves here keeps every path exactly where it was.
-- | The dashboard's own API surface: login/session plus the merchant-city
-- lookup. The transaction listing lives in 'API.Dashboard.TransactionView',
-- reached through 'API.DashboardLogin'.
module API.Dashboard
  ( API,
    handler,
  )
where

import qualified API.Dashboard.MerchantCityList as MerchantCityList
import qualified API.DashboardLogin as DashboardLogin
import Kernel.Types.App (FlowServerR)
import Kernel.Types.Flow (FlowR)
import Servant
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow)

type API =
  DashboardLogin.API
    :<|> MerchantCityList.API

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  DashboardLogin.handler
    :<|> MerchantCityList.handler
