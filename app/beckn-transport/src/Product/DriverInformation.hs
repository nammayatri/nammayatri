{-# LANGUAGE OverloadedLabels #-}

module Product.DriverInformation where

import qualified App.Types as App
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Types.Storage.RegistrationToken (RegistrationToken)
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Clock (NominalDiffTime)
import EulerHS.Prelude
import qualified Models.ProductInstance as ModelPI
import qualified Storage.Queries.DriverStats as QDriverStats
import Storage.Queries.Person (findAllActiveDrivers)
import qualified Storage.Queries.ProductInstance as QueryPI
import Types.API.DriverInformation (ActiveDriversResponse (..), DriverInformation (..))
import Types.App (DriverId (..))
import qualified Types.Storage.DriverStats as DriverStats

data ServiceHandle m = ServiceHandle
  { findActiveDrivers :: m [Person.Person],
    findRidesByStartTimeBuffer :: UTCTime -> NominalDiffTime -> [PI.ProductInstanceStatus] -> m [PI.ProductInstance],
    getCurrentTime :: m UTCTime,
    fetchDriversStats :: [DriverId] -> m [DriverStats.DriverStats]
  }

getAvailableDriversInfo :: RegistrationToken -> App.FlowHandler ActiveDriversResponse
getAvailableDriversInfo _ = do
  let handle =
        ServiceHandle
          { findActiveDrivers = findAllActiveDrivers,
            findRidesByStartTimeBuffer = ModelPI.findByStartTimeBuffer Case.RIDEORDER,
            getCurrentTime = getCurrTime,
            fetchDriversStats = QDriverStats.findByIds
          }
  withFlowHandler $ handleGetAvailableDriversInfo handle

handleGetAvailableDriversInfo :: (Monad m) => ServiceHandle m -> m ActiveDriversResponse
handleGetAvailableDriversInfo ServiceHandle {..} = do
  activeDriversIds <- fmap Person._id <$> findActiveDrivers
  now <- getCurrentTime
  freeDriversIds <- fetchFreeDriversIds activeDriversIds now
  driversInfo <- fetchDriversStats $ map (DriverId . _getPersonId) freeDriversIds
  pure $ ActiveDriversResponse {time = timePeriod, active_drivers = map mapToResp driversInfo}
  where
    fetchFreeDriversIds activeDriversIds time = do
      ridesBuffer <- findRidesByStartTimeBuffer time 1 [PI.CONFIRMED, PI.INPROGRESS, PI.TRIP_ASSIGNED]
      let busyDriversIds = catMaybes $ PI._personId <$> ridesBuffer
      let freeDriversIds = filter (`notElem` busyDriversIds) activeDriversIds
      pure freeDriversIds
    timePeriod = nominalDay -- Move into config if there will be a need
    mapToResp DriverStats.DriverStats {..} =
      DriverInformation
        { driver_id = PersonId . _getDriverId $ _driverId,
          completed_rides_over_time = _completedRidesNumber,
          earnings_over_time = fromRational $ toRational _earnings
        }

updateDriverInfo :: Text -> Integer -> App.FlowHandler APIResult.APIResult
updateDriverInfo _auth quantityToUpdate = withFlowHandler $ do
  driversIdsWithInfo <- fmap DriverStats._driverId <$> QDriverStats.fetchMostOutdatedDriversInfo quantityToUpdate
  now <- getCurrTime
  let fromTime = addUTCTime (- timePeriod) now
  driversInfo <- traverse (fetchDriverInfoById fromTime now) driversIdsWithInfo
  traverse_ updateInfo driversInfo
  pure APIResult.Success
  where
    fetchDriverInfoById fromTime toTime driverId = do
      rides <- QueryPI.getDriverCompletedRides (PersonId $ driverId ^. #_getDriverId) fromTime toTime
      let earnings = foldr sumProductInstancesByPrice 0 rides
      pure (driverId, length rides, earnings)
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
    updateInfo (driverId, completedRides, earnings) = QDriverStats.update driverId completedRides earnings
    timePeriod = nominalDay -- Move into config if there will be a need
