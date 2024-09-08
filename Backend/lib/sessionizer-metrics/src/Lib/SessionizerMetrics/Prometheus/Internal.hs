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
import Prometheus as P

type EventCounterMetric = P.Vector P.Label3 P.Counter

data EventCountersContainer = EventCountersContainer
  { eventRequestCounter :: EventCounterMetric,
    callAttemptCounter :: EventCounterMetric
  }

registerEventRequestCounterMetric :: IO EventCountersContainer
registerEventRequestCounterMetric = do
  eventRequestCounter <-
    P.register $
      P.vector ("event", "merchant_name", "version") $
        P.counter $ P.Info "event_count" ""
  callAttemptCounter <-
    P.register $
      P.vector ("event", "merchant_op_city_id", "version") $
        P.counter $ P.Info "event_count" ""
  return EventCountersContainer {eventRequestCounter, callAttemptCounter}

incrementEventRequestCounter ::
  ( MonadReader r m,
    MonadIO m
  ) =>
  (r -> EventCounterMetric) ->
  Text ->
  Text ->
  Text ->
  m ()
incrementEventRequestCounter getEventCounter merchantId event deploymentVersion = do
  counter' <- asks getEventCounter
  liftIO $ P.withLabel counter' (event, merchantId, deploymentVersion) P.incCounter

incrementCallAttemptCounter ::
  ( MonadReader r m,
    MonadIO m
  ) =>
  (r -> EventCounterMetric) ->
  Text ->
  Text ->
  Text ->
  m ()
incrementCallAttemptCounter getEventCounter merchantOpCityId event deploymentVersion = do
  counter' <- asks getEventCounter
  liftIO $ P.withLabel counter' (event, merchantOpCityId, deploymentVersion) P.incCounter
