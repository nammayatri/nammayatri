{-# LANGUAGE OverloadedLabels #-}

module Product.DriversInformation (handleActiveDrivers) where

import qualified App.Types as App
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.RegistrationToken (RegistrationToken)
import Beckn.Utils.Common (checkDBError, fromMaybeM500, getCurrTime, withFlowHandler)
import Data.Time (addUTCTime, diffUTCTime, nominalDay)
import EulerHS.Prelude
import Storage.Queries.Person (findAllActiveDrivers)
import Storage.Queries.ProductInstance (getDriverRides)
import Types.API.DriversInformation (ActiveDriversResponse (..), DriverInformation (..))

handleActiveDrivers :: RegistrationToken -> App.FlowHandler ActiveDriversResponse
handleActiveDrivers _ = withFlowHandler $ do
  activeDriversIds <- fmap Person._id <$> findAllActiveDrivers
  now <- getCurrTime
  let fromTime = addUTCTime (- timePeriod) now
  activeDrivers <- traverse (driverInfoById fromTime now) activeDriversIds
  return $
    ActiveDriversResponse
      { active_drivers = activeDrivers,
        time_period = timePeriod
      }
  where
    driverInfoById fromTime toTime driverId = do
      rides <- checkDBError =<< getDriverRides driverId fromTime toTime
      totalRideTime <- foldrM sumProductInstancesByTime 0 rides
      return $
        DriverInformation
          { driver_id = driverId,
            completed_rides_over_time = length rides,
            earnings_over_time = foldr sumProductInstancesByPrice 0 rides,
            total_ride_time = totalRideTime
          }
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
    sumProductInstancesByTime inst acc = do
      endTime <- inst ^. #_endTime & fromMaybeM500 "COMPLETED_RIDE_HAS_NO_END_TIME"
      return $ acc + diffUTCTime endTime (inst ^. #_startTime)
    timePeriod = nominalDay
