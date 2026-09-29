{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareAdjustment where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data FareAdjustment = FareAdjustment
  { areas :: Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.Area],
    baseFareScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    congestionScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    createdBy :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FareAdjustment.FareAdjustment,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    mode :: Domain.Types.FareAdjustment.FareAdjustmentMode,
    perKmRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perMinRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    reason :: Kernel.Prelude.Text,
    rolloutPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.FareAdjustment.FareAdjustmentStatus,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleServiceTiers :: [Domain.Types.Common.ServiceTierType],
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareAdjustmentMode = EXPERIMENT | SPIKE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FareAdjustmentStatus = DRAFT | ACTIVE | ENDED | EXPIRED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FareAdjustmentMode)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FareAdjustmentStatus)
