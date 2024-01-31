{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.LocationUpdate.Processor
  ( processLocationData,
    createDriverIdTokenKey,
  )
where

import qualified Consumer.AvailabilityTime.Types as T
import qualified Consumer.LocationUpdate.Types as LT
import Data.Time
import Data.Time.Clock.POSIX
import Environment
import EulerHS.Prelude hiding (toStrict)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Logging (logDebug)

processLocationData :: T.LocationUpdates -> T.DriverId -> Flow ()
processLocationData T.LocationUpdates {..} driverId = do
  let newUpdatedAt = ts
  logDebug $ "driver updated time " <> show newUpdatedAt <> driverId
  let encodedVal = createDriverIdTokenKey driverId
  Redis.zAdd "driver-last-location-update" [(utcToDouble newUpdatedAt, encodedVal)]

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds

createDriverIdTokenKey :: T.DriverId -> LT.DriverIdTokenKey
createDriverIdTokenKey driverId =
  LT.DriverIdTokenKey
    { driverId = driverId
    }
