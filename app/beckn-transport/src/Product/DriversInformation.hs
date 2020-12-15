{-# LANGUAGE OverloadedLabels #-}

module Product.DriversInformation where

import qualified App.Types as App
import Beckn.Types.App (PersonId)
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Types.Storage.RegistrationToken (RegistrationToken)
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Clock (NominalDiffTime)
import EulerHS.Prelude hiding (Handle)
import qualified Models.ProductInstance as ModelPI
import Storage.Queries.Person (findAllActiveDrivers)
import qualified Storage.Queries.ProductInstance as QueryPI
import Types.API.DriversInformation (ActiveDriversResponse (..), DriverInformation (..))

data Handle m = Handle
  { findActiveDrivers :: m [Person.Person],
    findRidesByStartTimeBuffer :: UTCTime -> NominalDiffTime -> [PI.ProductInstanceStatus] -> m [PI.ProductInstance],
    getCurrentTime :: m UTCTime,
    getDriverRidesInPeriod :: PersonId -> UTCTime -> UTCTime -> m [PI.ProductInstance]
  }

handleActiveDrivers :: RegistrationToken -> App.FlowHandler ActiveDriversResponse
handleActiveDrivers _ = do
  let handle =
        Handle
          { findActiveDrivers = findAllActiveDrivers,
            findRidesByStartTimeBuffer = ModelPI.findByStartTimeBuffer Case.RIDEORDER,
            getCurrentTime = getCurrTime,
            getDriverRidesInPeriod = QueryPI.getDriverRides
          }
  withFlowHandler $ execute handle

execute :: (Monad m) => Handle m -> m ActiveDriversResponse
execute Handle {..} = do
  activeDriversIds <- fmap Person._id <$> findActiveDrivers
  now <- getCurrentTime
  freeDriversIds <- fetchFreeDriversIds activeDriversIds now
  let fromTime = addUTCTime (- timePeriod) now
  activeDriversInfo <- traverse (driverInfoById fromTime now) freeDriversIds
  pure $ ActiveDriversResponse {time = timePeriod, active_drivers = activeDriversInfo}
  where
    fetchFreeDriversIds activeDriversIds time = do
      ridesBuffer <- findRidesByStartTimeBuffer time 1 [PI.CONFIRMED, PI.INPROGRESS, PI.TRIP_ASSIGNED]
      let busyDriversIds = catMaybes $ PI._personId <$> ridesBuffer
      let freeDriversIds = filter (`notElem` busyDriversIds) activeDriversIds
      pure freeDriversIds
    driverInfoById fromTime toTime driverId = do
      rides <- getDriverRidesInPeriod driverId fromTime toTime
      pure $
        DriverInformation
          { driver_id = driverId,
            completed_rides_over_time = length rides,
            earnings_over_time = foldr sumProductInstancesByPrice 0 rides
          }
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
    timePeriod = nominalDay -- Move into config if there will be a need
