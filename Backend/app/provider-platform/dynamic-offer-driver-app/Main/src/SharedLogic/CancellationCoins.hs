module SharedLogic.CancellationCoins where

import Data.Aeson
import Data.Default.Class
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT

data CancellationCoinResult = CancellationCoinResult
  { coins :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data CancellationCoinData = CancellationCoinData
  { cancelledBy :: DCT.CancellationType,
    timeOfDriverCancellation :: Int,
    timeOfCustomerCancellation :: Int,
    isArrivedAtPickup :: Bool,
    driverWaitingTime :: Maybe Int,
    callAttemptByDriver :: Bool,
    actualCoveredDistance :: Maybe Meters,
    expectedCoveredDistance :: Maybe Meters
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Default CancellationCoinResult where
  def =
    CancellationCoinResult {coins = 0}

instance Default CancellationCoinData where
  def =
    CancellationCoinData
      { cancelledBy = DCT.CancellationByCustomer,
        timeOfDriverCancellation = 0,
        timeOfCustomerCancellation = 0,
        isArrivedAtPickup = False,
        driverWaitingTime = Nothing,
        callAttemptByDriver = False,
        actualCoveredDistance = Nothing,
        expectedCoveredDistance = Nothing
      }
