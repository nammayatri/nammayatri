{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Login, two-factor, user administration, roles, merchants, capabilities and
-- resource scope -- the tree that issues and manages the session every other
-- dashboard route is authorised against.
--
-- It lives in lib-dashboard, which links no application package, so an
-- application server can serve it directly instead of proxying through
-- provider-dashboard. Every handler is polymorphic in the server environment
-- via 'DashboardLoginFlow'.
--
-- The routes that name the access matrix or the transaction endpoint enum are
-- not here: those types are promoted into provider-dashboard's generated proxy
-- tree and stay in lib-dashboard-api, mounted alongside this tree by
-- 'API.Dashboard'.
module API.DashboardLogin
  ( API,
    handler,
  )
where

import qualified API.Dashboard.Capability as Capability
import qualified API.Dashboard.EmailVerification as EmailVerification
import qualified API.Dashboard.Entity as Entity
import qualified API.Dashboard.Merchant as Merchant
import qualified API.Dashboard.Person as Person
import qualified API.Dashboard.PersonBulk as PersonBulk
import qualified API.Dashboard.Registration as Registration
import qualified API.Dashboard.ResourceScope as ResourceScope
import qualified API.Dashboard.Roles as Roles
import qualified API.Dashboard.SpecialZone as SpecialZone
import qualified API.Dashboard.TransactionView as TransactionView
import Kernel.Types.App (FlowServerR)
import Kernel.Types.Flow (FlowR)
import Servant
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow)

type API =
  Person.API
    :<|> Registration.API
    :<|> EmailVerification.API
    :<|> Roles.API
    :<|> Merchant.API
    :<|> Capability.API
    :<|> ResourceScope.API
    :<|> TransactionView.API
    :<|> Entity.API
    :<|> PersonBulk.API
    :<|> SpecialZone.API

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  Person.handler
    :<|> Registration.handler
    :<|> EmailVerification.handler
    :<|> Roles.handler
    :<|> Merchant.handler
    :<|> Capability.handler
    :<|> ResourceScope.handler
    :<|> TransactionView.handler
    :<|> Entity.handler
    :<|> PersonBulk.handler
    :<|> SpecialZone.handler
