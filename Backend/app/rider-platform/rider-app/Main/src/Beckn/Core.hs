{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Beckn.Core where

import Data.List (lookup)
import qualified Data.Text.Encoding as T
import EulerHS.Prelude
import qualified Kernel.Storage.Queries.BecknRequest as QBR
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import qualified Network.Wai.Internal as Wai
import Servant
import Storage.Beam.BecknRequest ()

logBecknRequest ::
  (HasField "coreMetrics" f CoreMetricsContainer) =>
  (HasField "loggerEnv" f LoggerEnv) =>
  (HasField "version" f DeploymentVersion) =>
  EnvR f ->
  Application ->
  Application
logBecknRequest (EnvR flowRt appEnv) f req@Wai.Request {..} respF = do
  req' <- case lookup "Authorization" requestHeaders of
    Nothing -> do
      void $
        runFlowR flowRt appEnv $ do
          logDebug "no logBecknRequest"
      return req
    Just header -> do
      body <- requestBody
      bodyMvar <- newMVar body
      void $
        runFlowR flowRt appEnv $ do
          logDebug "logBecknRequest"
          QBR.logBecknRequest (T.decodeUtf8 body) (T.decodeUtf8 header)
      return req {Wai.requestBody = mkRequestBody bodyMvar}
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe mempty
