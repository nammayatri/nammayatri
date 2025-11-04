module Domain.Types.UpgradedTier where

import Domain.Types.ServiceTierType
import Kernel.Prelude

data UpgradedTier = UpgradedTier
  { lastComputed :: UTCTime,
    tier :: ServiceTierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
