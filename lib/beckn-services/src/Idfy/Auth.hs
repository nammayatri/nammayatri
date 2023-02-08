module Idfy.Auth where

import qualified Data.Map.Strict as Map
import EulerHS.Prelude
import Idfy.Types.IdfyConfig
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.IOLogging
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http

verifyAuth ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    Log (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Maybe Text ->
  FlowR a ()
verifyAuth authSecret = do
  idfyConfig <- asks (.idfyCfg)
  unless (authSecret == (Just idfyConfig.secret)) $ throwError (InvalidRequest "INVALID_AUTHORIZATION_HEADER")

prepareIdfyHttpManager :: Int -> Map String Http.ManagerSettings
prepareIdfyHttpManager timeout =
  Map.singleton idfyHttpManagerKey $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

idfyHttpManagerKey :: String
idfyHttpManagerKey = "idfy-http-manager"
