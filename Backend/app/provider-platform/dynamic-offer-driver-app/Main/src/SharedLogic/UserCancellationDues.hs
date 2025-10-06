module SharedLogic.UserCancellationDues where

import Data.Default.Class
import qualified Domain.Types as DTC
import qualified Domain.Types.ServiceTierType as DServiceTierType
import Kernel.Prelude
import Kernel.Types.Common
import qualified Lib.DriverCoins.Types as DCT

data UserCancellationDuesData = UserCancellationDuesData
  { cancelledBy :: DCT.CancellationType,
    timeOfDriverCancellation :: Int,
    timeOfCustomerCancellation :: Int,
    isArrivedAtPickup :: Bool,
    driverWaitingTime :: Maybe Int,
    callAttemptByDriver :: Bool,
    actualCoveredDistance :: Maybe Meters,
    expectedCoveredDistance :: Maybe Meters,
    cancellationDues :: HighPrecMoney,
    cancelledRides :: Int,
    totalBookings :: Int,
    completedRides :: Int,
    validCancellations :: Int,
    cancellationDueRides :: Int,
    serviceTier :: DServiceTierType.ServiceTierType,
    tripCategory :: DTC.TripCategory
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default UserCancellationDuesData where
  def =
    UserCancellationDuesData
      { cancelledBy = DCT.CancellationByCustomer,
        timeOfDriverCancellation = 0,
        timeOfCustomerCancellation = 0,
        isArrivedAtPickup = False,
        driverWaitingTime = Nothing,
        callAttemptByDriver = False,
        actualCoveredDistance = Nothing,
        expectedCoveredDistance = Nothing,
        cancellationDues = 0,
        cancelledRides = 0,
        totalBookings = 0,
        completedRides = 0,
        validCancellations = 0,
        cancellationDueRides = 0,
        serviceTier = DServiceTierType.TAXI,
        tripCategory = DTC.OneWay DTC.OneWayOnDemandDynamicOffer
      }

newtype UserCancellationDuesResult = UserCancellationDuesResult
  { cancellationCharges :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default UserCancellationDuesResult where
  def = UserCancellationDuesResult {cancellationCharges = 0}
