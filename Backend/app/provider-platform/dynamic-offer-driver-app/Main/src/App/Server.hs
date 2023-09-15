{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module App.Server where

import API
import Beckn.Core (logBecknRequest)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Text as T (pack)
import Data.UUID.V4 (nextRandom)
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Prelude hiding (unpack)
import qualified EulerHS.Runtime as R
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion (..))
import Kernel.Tools.Metrics.Init
import Kernel.Types.App
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.IOLogging (HasLog, appendLogTag)
import Kernel.Utils.Monitoring.Prometheus.Servant ()
import qualified Kernel.Utils.Servant.Server as BU
import Kernel.Utils.Shutdown
import qualified Kernel.Utils.SignatureAuth as HttpSig
import qualified Kernel.Utils.Time as TT
import Network.HTTP.Types (Method, RequestHeaders)
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai as Wai
import Network.Wai.Internal
import Servant
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import Tools.Auth

logRequestAndResponse' :: HasLog f => EnvR f -> Application -> Application
logRequestAndResponse' (EnvR flowRt appEnv) =
  logRequestAndResponseGeneric' logInfoIO
  where
    logInfoIO tag info = runFlowR flowRt appEnv $ logTagInfo tag info

logRequestAndResponseGeneric' :: (Text -> Text -> IO ()) -> Application -> Application
logRequestAndResponseGeneric' logInfoIO f req respF =
  md $ f req loggedRespF
  where
    md k = do
      (res, duration) <- TT.measureDuration k
      logInfoIO "measuringAllFunctions : Duration : " $ show duration
      return res
    loggedRespF resp = do
      logInfoIO "Request&Response full" $ "Request: " <> show req
      respF resp

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run driverOfferAPI driverOfferServer context modifiedEnv
    & logRequestAndResponse' modifiedEnv
    & logBecknRequest modifiedEnv
    & addServantInfo modifiedEnv.appEnv.version driverOfferAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext
