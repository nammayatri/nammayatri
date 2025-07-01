{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Fleet where

import qualified API.Dashboard.Fleet.BulkAssociation as BulkAssociation
import qualified API.Dashboard.Fleet.Registration as Registration
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth

type API =
  DashboardTokenAuth
    :> (Registration.API :<|> "fleet" :> BulkAssociation.API)

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city _ = Registration.handler merchantId city :<|> BulkAssociation.handler merchantId city
