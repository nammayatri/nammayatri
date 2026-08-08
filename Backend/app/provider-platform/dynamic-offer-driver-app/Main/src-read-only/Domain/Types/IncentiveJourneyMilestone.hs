{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IncentiveJourneyMilestone where

import Data.Aeson
import qualified Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data IncentiveJourneyMilestone = IncentiveJourneyMilestone
  { conditionOperator :: Kernel.Prelude.Maybe Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    journeyId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney,
    order :: Kernel.Prelude.Int,
    pickupSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    rewardConfigId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Coins.CoinsConfig.CoinsConfig),
    rewardType :: Domain.Types.IncentiveJourneyMilestone.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MilestoneConditionOperator = GTE | GT | EQ | LTE | LT | CT deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

data MilestoneConditionType
  = RideCompleted
  | Earnings
  | Distance
  | RideDuration
  | PickupSpecialLocation
  | DropSpecialLocation
  | PickupDropSpecialLocation
  deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

data MilestoneRewardType = Coins | Cash | Coupons deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MilestoneConditionType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MilestoneConditionType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MilestoneConditionOperator)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MilestoneConditionOperator)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MilestoneRewardType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MilestoneRewardType)
