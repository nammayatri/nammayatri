module Domain.Types.TransporterConfig where

import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.External.FCM.Types (FCMConfig)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC

-- ProviderConfig?
data TransporterConfigD u = TransporterConfig
  { merchantId :: Id Merchant,
    pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    rideTravelledDistThresholdWhenPickupOrDestIsDiff :: Maybe Meters,
    rideTravelledDistThresholdWhenPickupAndDestIsSame :: Maybe Meters,
    rideTimeEstimatedThreshold :: Maybe Seconds,
    availabilityTimeWeightage :: Int,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: Int,
    acceptanceRatioWindowOption :: SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Int,
    cancellationRatioWindowOption :: SWC.SlidingWindowOptions,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    fcmConfig :: FCMConfig
  }
  deriving (Generic, Show)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)
