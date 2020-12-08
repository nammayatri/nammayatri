{-# LANGUAGE OverloadedLabels #-}

module Product.DriversInformation (handleActiveDrivers) where

import qualified App.Types as App
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (checkDBError, withFlowHandler)
import EulerHS.Prelude
import Storage.Queries.Person (findAllActiveDrivers)
import Storage.Queries.ProductInstance (findCompletedRidesOver24hByDriverId)
import Types.API.DriversInformation (ActiveDriversResponse (..), DriverInformation (..))

handleActiveDrivers :: App.FlowHandler ActiveDriversResponse
handleActiveDrivers = withFlowHandler $ do
  activeDrivers <- findAllActiveDrivers
  let activeDriversIds = Person._id <$> activeDrivers
  ActiveDriversResponse <$> traverse driverInfoById activeDriversIds
  where
    driverInfoById driverId = do
      rides <- checkDBError =<< findCompletedRidesOver24hByDriverId driverId
      return $
        DriverInformation
          { driver_id = driverId,
            completed_rides_over_24h = length rides,
            earnings_over_24h = foldr sumProductInstancesByPrice 0 rides
          }
    sumProductInstancesByPrice inst acc = acc + fromRational (toRational (inst ^. #_price))
