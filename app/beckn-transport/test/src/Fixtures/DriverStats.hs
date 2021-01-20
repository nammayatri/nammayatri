module Fixtures.DriverStats (defaultDriverStats, mkDriverStats) where

import qualified Beckn.Types.Amount as Amount
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Types.App (DriverId (..))
import qualified Types.Storage.DriverStats as DriverStats

defaultDriverStats :: DriverStats.DriverStats
defaultDriverStats =
  DriverStats.DriverStats
    { _driverId = DriverId "1",
      _completedRidesNumber = 2,
      _earnings = Amount.Amount $ 200 % 1,
      _lastRideAt = Fixtures.defaultTime,
      _updatedAt = Fixtures.defaultTime,
      _createdAt = Fixtures.defaultTime
    }

mkDriverStats :: Text -> Int -> Integer -> DriverStats.DriverStats
mkDriverStats driverId completedRidesNum earnings =
  DriverStats.DriverStats
    { _driverId = DriverId driverId,
      _completedRidesNumber = completedRidesNum,
      _earnings = Amount.Amount $ earnings % 1,
      _lastRideAt = DriverStats.distantPast,
      _updatedAt = Fixtures.defaultTime,
      _createdAt = Fixtures.defaultTime
    }
