{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard
  ( API,
    handler,
  )
where

import "lib-dashboard" API.Dashboard (API)
import qualified API.Dashboard.AccessMatrix as AccessMatrix
import qualified API.Dashboard.Merchant as Merchant
import qualified API.Dashboard.Person as Person
import qualified API.Dashboard.Registration as Registration
import qualified API.Dashboard.Roles as Roles
import "lib-dashboard" Environment
import Servant

--  Note : Type of API is defined in lib-dashboard/API/Dashboard.hs

handler :: FlowServer API
handler =
  Person.handler
    :<|> Registration.handler
    :<|> AccessMatrix.handler
    :<|> Roles.handler
    :<|> Merchant.handler
