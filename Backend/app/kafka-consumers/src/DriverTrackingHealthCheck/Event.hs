{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DriverTrackingHealthCheck.Event where

import Environment (Flow)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude

data DriverHealthCheckPingEvent = DriverHealthCheckPingEvent
  { driver_id :: Text,
    ts :: UTCTime,
    merchant_op_city_id :: Text,
    ping_count :: Int,
    mode :: Text,
    eventType :: Text
  }
  deriving (Generic, ToJSON)

pushHealthCheckPingEvent :: Text -> UTCTime -> Text -> Int -> Text -> Flow ()
pushHealthCheckPingEvent driverId now cityId count driverMode =
  pushToKafka
    DriverHealthCheckPingEvent
      { driver_id = driverId,
        ts = now,
        merchant_op_city_id = cityId,
        ping_count = count,
        mode = driverMode,
        eventType = "TRIGGER_SERVICE"
      }
    "driver-health-check-ping"
    driverId
