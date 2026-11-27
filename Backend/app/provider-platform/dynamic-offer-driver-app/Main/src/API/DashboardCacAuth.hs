{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Kept after haskell-cac removal for backward compatibility: older CAC
-- clients may still call /CAC/auth; dropping the route would 404 them.
-- Real token→ACL lookup is gone — always return a stub CacUser.
module API.DashboardCacAuth
  ( API,
    handler,
  )
where

import qualified "this" Dashboard.ProviderPlatform.CacAuth as Common
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant

type API =
  "CAC"
    :> CacAuthAPI

type CacAuthAPI = Common.CacAuthAPI

handler :: FlowServer API
handler _ _ = cacAuthHandler

cacAuthHandler :: Maybe String -> FlowHandler Common.CacUser
cacAuthHandler mbToken =
  withFlowHandlerAPI' $
    pure
      Common.CacUser
        { username = "admin",
          email = "admin@juspay.in",
          acl = "{}",
          token = fromMaybe "" mbToken,
          auth_type = "Bearer"
        }
