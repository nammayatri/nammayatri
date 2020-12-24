{-# LANGUAGE OverloadedLabels #-}

module Product.DriverInformation where

import qualified App.Types as App
import qualified Beckn.Types.Ack as Ack
import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Types.Storage.RegistrationToken (RegistrationToken)
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Clock (NominalDiffTime)
import EulerHS.Prelude hiding (Handle)
import qualified Models.ProductInstance as ModelPI
import qualified Storage.Queries.DriverInformation as QueryDI
import Storage.Queries.Person (findAllActiveDrivers)
import qualified Storage.Queries.ProductInstance as QueryPI
import Types.API.DriverInformation (ActiveDriversResponse (..), DriverInformation (..))
import Types.App (DriverId (..))
import qualified Types.Storage.DriverInformation as DI

data Handle m = Handle
  { findActiveDrivers :: m [Person.Person],
    findRidesByStartTimeBuffer :: UTCTime -> NominalDiffTime -> [PI.ProductInstanceStatus] -> m [PI.ProductInstance],
    getCurrentTime :: m UTCTime,
    fetchDriversInfo :: [DriverId] -> m [DI.DriverInformation]
  }

handleActiveDrivers :: RegistrationToken -> App.FlowHandler ActiveDriversResponse
handleActiveDrivers _ = do
  let handle =
        Handle
          { findActiveDrivers = findAllActiveDrivers,
            findRidesByStartTimeBuffer = ModelPI.findByStartTimeBuffer Case.RIDEORDER,
            getCurrentTime = getCurrTime,
            fetchDriversInfo = QueryDI.findByIds
          }
  withFlowHandler $ execute handle

execute :: (Monad m) => Handle m -> m ActiveDriversResponse
execute Handle {..} = do
  activeDriversIds <- fmap Person._id <$> findActiveDrivers
  now <- getCurrentTime
  freeDriversIds <- fetchFreeDriversIds activeDriversIds now
  driversInfo <- fetchDriversInfo $ map (DriverId . _getPersonId) freeDriversIds
  pure $ ActiveDriversResponse {time = timePeriod, active_drivers = map mapToResp driversInfo}
  where
    fetchFreeDriversIds activeDriversIds time = do
      ridesBuffer <- findRidesByStartTimeBuffer time 1 [PI.CONFIRMED, PI.INPROGRESS, PI.TRIP_ASSIGNED]
      let busyDriversIds = catMaybes $ PI._personId <$> ridesBuffer
      let freeDriversIds = filter (`notElem` busyDriversIds) activeDriversIds
      pure freeDriversIds
    timePeriod = nominalDay -- Move into config if there will be a need
    mapToResp DI.DriverInformation {..} =
      DriverInformation
        { driver_id = PersonId . _getDriverId $ _driverId,
          completed_rides_over_time = _completedRidesNumber,
          earnings_over_time = fromRational $ toRational _earnings
        }

updateDriverInfo :: Text -> Integer -> App.FlowHandler Ack.Ack
updateDriverInfo _auth quantityToUpdate = withFlowHandler $ do
  driversIdsWithInfo <- fmap DI._driverId <$> QueryDI.fetchMostOutdatedDriversInfo quantityToUpdate
  now <- getCurrTime
  let fromTime = addUTCTime (- timePeriod) now
  driversInfo <- traverse (fetchDriverInfoById fromTime now) driversIdsWithInfo
  traverse_ updateInfo driversInfo
  pure Ack.ackSuccess
  where
    fetchDriverInfoById fromTime toTime driverId = do
      rides <- QueryPI.getDriverCompletedRides (PersonId $ driverId ^. #_getDriverId) fromTime toTime
      let earnings = foldr sumProductInstancesByPrice 0 rides
      pure (driverId, length rides, earnings)
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
    updateInfo (driverId, completedRides, earnings) = QueryDI.update driverId completedRides earnings
    timePeriod = nominalDay -- Move into config if there will be a need
