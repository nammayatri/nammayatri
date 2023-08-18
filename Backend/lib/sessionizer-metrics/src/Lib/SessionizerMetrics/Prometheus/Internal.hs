{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.SessionizerMetrics.Prometheus.Internal where

import Kernel.Prelude
import Kernel.Types.Common
-- import Kernel.Utils.Common

-- import Lib.SessionizerMetrics.Types.Event

import Kernel.Utils.Common (logDebug)
import Lib.SessionizerMetrics.Prometheus.CounterConfig
import Prometheus as P

incrementCounter ::
  ( MonadReader r1 m,
    MonadGuid m,
    MonadTime m,
    HasField "getDeploymentVersion" r2 Text,
    HasField "version" r1 r2,
    MonadIO m,
    Log m
  ) =>
  P.Vector P.Label3 P.Counter ->
  PrometheusCounterConfig ->
  m ()
incrementCounter counterName promConfig = do
  logDebug " Rupak Prometheus Increment Counter"
  version <- asks (.version)
  let event = promConfig.event
  let merchantName = promConfig.merchantName
  liftIO $ P.withLabel counterName (event, merchantName, version.getDeploymentVersion) P.incCounter

type EventCounterMetric = P.Vector P.Label3 P.Counter

registerEventRequestCounterMetric :: IO EventCounterMetric
registerEventRequestCounterMetric = P.register $ P.vector ("event", "merchant_name", "version") $ P.counter $ P.Info "event_count" ""
