{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MgCampaignRule where

import Data.Aeson
import qualified Domain.Types.MgCampaign
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MgCampaignRule = MgCampaignRule
  { campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    id :: Kernel.Types.Id.Id Domain.Types.MgCampaignRule.MgCampaignRule,
    metric :: Domain.Types.MgCampaignRule.MgMetric,
    params :: Kernel.Prelude.Maybe Domain.Types.MgCampaignRule.MgRuleParams,
    slabs :: [Domain.Types.MgCampaignRule.MgSlab],
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data MgMetric = CANCELLATIONS | BREAK_MINUTES | RIDES_COMPLETED | DISTANCE_KM deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data MgRuleParams = MgRuleParams {minRideDistanceMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Int, minRideFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MgSlab = MgSlab {fromValue :: Kernel.Prelude.Int, percent :: Kernel.Prelude.Int} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgMetric)
