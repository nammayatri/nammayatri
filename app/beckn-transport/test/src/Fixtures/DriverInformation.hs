module Fixtures.DriverInformation (defaultDriverInformation, mkDriverInfo) where

import qualified Beckn.Types.Amount as Amount
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Types.App (DriverId (..))
import qualified Types.Storage.DriverInformation as DI

defaultDriverInformation :: DI.DriverInformation
defaultDriverInformation =
  DI.DriverInformation
    { _driverId = DriverId "1",
      _completedRidesNumber = 2,
      _earnings = Amount.Amount $ 200 % 1,
      _updatedAt = Fixtures.defaultTime
    }

mkDriverInfo :: Text -> Int -> Integer -> DI.DriverInformation
mkDriverInfo driverId completedRidesNum earnings =
  DI.DriverInformation
    { _driverId = DriverId driverId,
      _completedRidesNumber = completedRidesNum,
      _earnings = Amount.Amount $ earnings % 1,
      _updatedAt = Fixtures.defaultTime
    }
