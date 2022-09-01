module Idfy.Auth where

import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Types.Error
import Beckn.Types.Flow
import Beckn.Utils.Common
import Beckn.Utils.IOLogging
import EulerHS.Prelude
import Idfy.Types.IdfyConfig

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
