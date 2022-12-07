module Domain.Types.TransporterConfig where

import Beckn.External.FCM.Types (FCMConfig)
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)

-- ProviderConfig?
data TransporterConfigD u = TransporterConfig
  { merchantId :: Id Merchant,
    pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    rideTravelledDistanceThreshold :: Maybe Meters,
    rideTimeEstimatedThreshold :: Maybe Seconds,
    availabilityTimeWeightage :: Int,
    acceptanceRatioWeightage :: Int,
    cancellationRatioWeightage :: Int,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    fcmConfig :: FCMConfig
  }
  deriving (Generic, Show)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)
