module Domain.Types.TransporterConfig where

import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)

-- ProviderConfig?
data TransporterConfig = TransporterConfig
  { organizationId :: Id Organization,
    pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
