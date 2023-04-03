{-
 Copyright 2022-23, Juspay India Pvt Ltd

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

type DashboardTokenAuth = HeaderAuth "token" DashboardVerifyToken

data DashboardVerifyToken = DashboardVerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DashboardTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data Dashboard = Dashboard

instance VerificationMethod DashboardVerifyToken where
  type VerificationResult DashboardVerifyToken = Dashboard
  verificationDescription =
    "Checks whether dashboard token is registered."

verifyDashboardAction :: HasFlowEnv m r '["dashboardToken" ::: Text] => VerificationAction DashboardVerifyToken m
verifyDashboardAction = VerificationAction verifyDashboard

-- Do we need some expiry time for dashboard token?
verifyDashboard :: HasFlowEnv m r '["dashboardToken" ::: Text] => RegToken -> m Dashboard
verifyDashboard incomingToken = do
  dashboardToken <- asks (.dashboardToken)
  if incomingToken == dashboardToken
    then pure Dashboard
    else throwError (InvalidToken "dashboard token") -- we shouldn't show to dashboard user incoming token
