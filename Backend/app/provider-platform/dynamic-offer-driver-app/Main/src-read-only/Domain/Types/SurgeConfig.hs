{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SurgeConfig where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Extra.SurgeConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data SurgeConfig = SurgeConfig
  { applyOnExtraDistanceOnly :: Kernel.Prelude.Bool,
    createdBy :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    excludedAreas :: Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.Area],
    id :: Kernel.Types.Id.Id Domain.Types.SurgeConfig.SurgeConfig,
    maxDeltaPerUpdate :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    maxMultiplier :: Kernel.Types.Common.Centesimal,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minMultiplier :: Kernel.Types.Common.Centesimal,
    rows :: [Domain.Types.Extra.SurgeConfig.SurgeRow],
    status :: Domain.Types.SurgeConfig.SurgeConfigStatus,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    version :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SurgeConfigStatus = DRAFT | SHADOW | ACTIVE | ARCHIVED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SurgeConfigStatus)
