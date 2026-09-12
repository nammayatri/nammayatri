{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Token-to-ACL lookup for CAC. A temporary shim, to be dropped once CAC has
-- SSO -- carried over verbatim so nothing depends on provider-dashboard for it
-- in the meantime.
--
-- Touches no database: the map comes from AUTH_MAP at startup. Unlike
-- provider-dashboard, an unset AUTH_MAP is not fatal here (see Environment) --
-- it simply means no token matches.
module API.DashboardCacAuth
  ( API,
    handler,
  )
where

import qualified "this" Dashboard.ProviderPlatform.CacAuth as Common
import Data.Aeson as DA
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI')
import Kernel.Utils.Error.Throwing
import Servant hiding (throwError)
import "lib-dashboard" Tools.Error

type API =
  "CAC"
    :> CacAuthAPI

type CacAuthAPI = Common.CacAuthAPI

handler :: FlowServer API
handler _ _ = cacAuthHandler

cacAuthHandler :: Maybe String -> FlowHandler Common.CacUser
cacAuthHandler mbToken = withFlowHandlerAPI' $ do
  token <- fromMaybeM CacAuthError mbToken
  mbAcl <- lookup token <$> asks (.cacAclMap)
  case mbAcl of
    Just acl' -> do
      let acl = (TL.unpack . TLE.decodeUtf8 . DA.encode . HM.fromList) acl'
      return $
        Common.CacUser
          { username = "admin",
            email = "admin@juspay.in",
            auth_type = "Bearer",
            ..
          }
    Nothing -> throwError CacInvalidToken
