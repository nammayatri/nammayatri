{-
 Copyright 2024-25, MovingTech Innovation Tech Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth where

import Data.Text as T
import EulerHS.Prelude hiding (id)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common as CoreCommon
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (throwError)

type ApiTokenAuth = HeaderAuth "token" ApiVerifyToken

data ApiVerifyToken = ApiVerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ApiTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data Verified = Verified

instance VerificationMethod ApiVerifyToken where
  type VerificationResult ApiVerifyToken = Verified
  verificationDescription =
    "Checks whether api token is registered."

verifyAction :: HasFlowEnv m r '["apiToken" ::: Text] => VerificationAction ApiVerifyToken m
verifyAction = VerificationAction verify

-- Do we need some expiry time for api token?
verify :: HasFlowEnv m r '["apiToken" ::: Text] => RegToken -> m Verified
verify incomingToken = do
  apiToken <- asks (.apiToken)
  if incomingToken == apiToken
    then pure Verified
    else throwError (InvalidToken "api token") -- we shouldn't show to api user incoming token
