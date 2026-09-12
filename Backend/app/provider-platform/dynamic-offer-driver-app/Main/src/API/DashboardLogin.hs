{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The dashboard's login and user-administration tree, served directly.
--
-- provider-dashboard mounts these routes at its root (@\/user\/login@,
-- @\/admin\/person\/list@, ...). Here they sit under @\/direct-dashboard@ so they
-- cannot collide with this server's own routes, which means ingress maps them
-- with the same prefix rule it uses for the rest of the dashboard.
--
-- The handlers are the same code provider-dashboard runs -- lib-dashboard's
-- 'DashboardLogin.handler', polymorphic in the server environment -- with their
-- queries pointed at the dashboard database by 'withDashboardDbFlowHandlerAPI'.
module API.DashboardLogin
  ( API,
    handler,
  )
where

import qualified "lib-dashboard" API.DashboardLogin as DashboardLogin
import Environment
import Servant
-- Schema resolution for the dashboard tables. Orphan instances, defined once in
-- lib-dashboard; needed here because this is where the handlers are used.
import "lib-dashboard" Storage.Beam.SchemaInstances ()

type API = "direct-dashboard" :> DashboardLogin.API

handler :: FlowServer API
handler = DashboardLogin.handler
