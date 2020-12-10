{-# LANGUAGE OverloadedLabels #-}

module Product.DriversInformation where

import qualified App.Types as App
import Beckn.Types.App (PersonId)
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken (RegistrationToken)
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import EulerHS.Prelude hiding (Handle)
import Storage.Queries.Person (findAllActiveDrivers)
import Storage.Queries.ProductInstance (getDriverRides)
import Types.API.DriversInformation (ActiveDriversResponse, DriverInformation (..))

data Handle m = Handle
  { findActiveDrivers :: m [Person.Person],
    getCurrentTime :: m UTCTime,
    getDriverRidesInPeriod :: PersonId -> UTCTime -> UTCTime -> m [ProductInstance.ProductInstance]
  }

handleActiveDrivers :: RegistrationToken -> App.FlowHandler ActiveDriversResponse
handleActiveDrivers _ = do
  let handle =
        Handle
          { findActiveDrivers = findAllActiveDrivers,
            getCurrentTime = getCurrTime,
            getDriverRidesInPeriod = getDriverRides
          }
  withFlowHandler $ execute handle

execute :: (Monad m) => Handle m -> m ActiveDriversResponse
execute Handle {..} = do
  activeDriversIds <- fmap Person._id <$> findActiveDrivers
  now <- getCurrentTime
  let fromTime = addUTCTime (- timePeriod) now
  traverse (driverInfoById fromTime now) activeDriversIds
  where
    driverInfoById fromTime toTime driverId = do
      rides <- getDriverRidesInPeriod driverId fromTime toTime
      return $
        DriverInformation
          { driver_id = driverId,
            completed_rides_over_24h = length rides,
            earnings_over_24h = foldr sumProductInstancesByPrice 0 rides
          }
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
    timePeriod = nominalDay
