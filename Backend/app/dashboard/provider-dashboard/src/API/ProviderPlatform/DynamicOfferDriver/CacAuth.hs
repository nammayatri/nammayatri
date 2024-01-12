{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module API.ProviderPlatform.DynamicOfferDriver.CacAuth
  ( API,
    handler,
  )
where

import qualified Dashboard.ProviderPlatform.CacAuth as Common
import Data.Aeson as DA
import qualified Data.ByteString.Lazy.UTF8 as DBL
import qualified Data.HashMap.Strict as HM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI')
import Kernel.Utils.Error.Throwing
import Servant hiding (throwError)
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Error

-- This is a temporary implementation of the CAC auth API. This will be depcricated once we have SSO for CAC.

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
      let acl = (DBL.toString . DA.encode . HM.fromList) acl'
      return $
        Common.CacUser
          { username = "admin",
            email = "admin@juspay.in",
            auth_type = "Bearer",
            ..
          }
    Nothing -> throwError CacInvalidToken
