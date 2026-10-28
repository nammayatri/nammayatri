{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MgCampaign where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MgCampaign = MgCampaign
  { createdBy :: Kernel.Prelude.Text,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endDate :: Data.Time.Calendar.Day,
    forceAssign :: Kernel.Prelude.Bool,
    frequency :: Domain.Types.MgCampaign.MgFrequency,
    guaranteeAmount :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    maxBudget :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    onFleetChange :: Domain.Types.MgCampaign.MgFleetChangePolicy,
    optInCutoffMinutes :: Kernel.Prelude.Int,
    settlementGraceMinutes :: Kernel.Prelude.Int,
    startDate :: Data.Time.Calendar.Day,
    status :: Domain.Types.MgCampaign.MgCampaignStatus,
    vehicleCategories :: [Domain.Types.VehicleCategory.VehicleCategory],
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data MgCampaignStatus = DRAFT | PUBLISHED | PAUSED | CLOSED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data MgFleetChangePolicy = BLOCK | DISQUALIFY | CANCEL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data MgFrequency = DAILY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgCampaignStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgFleetChangePolicy)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgFrequency)
