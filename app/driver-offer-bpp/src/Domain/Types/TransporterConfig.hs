module Domain.Types.TransporterConfig where

import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)

-- ProviderConfig?
data TransporterConfigD u = TransporterConfig
  { organizationId :: Id Organization,
    pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    rideTravelledDistanceThreshold :: Maybe Meters,
    rideTimeEstimatedThreshold :: Maybe Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)
