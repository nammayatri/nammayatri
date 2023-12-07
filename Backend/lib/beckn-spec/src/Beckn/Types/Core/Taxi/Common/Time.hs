{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Time where

import Beckn.Types.Core.Taxi.Common.TimeDuration
import Beckn.Types.Core.Taxi.Common.TimeTimestamp
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State)

data Time = Time
  { label :: Text,
    timeStamp :: TimeTimestamp,
    duration :: TimeDuration,
    range :: Range,
    days :: String,
    schedule :: Schedule
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

data Range = Range
  { start :: TimeTimestamp,
    end :: TimeTimestamp
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

data Schedule = Schedule
  { frequency :: TimeDuration,
    hoidays :: Maybe [TimeTimestamp],
    times :: Maybe [TimeTimestamp]
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)
