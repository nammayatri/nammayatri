{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IncentiveJourneyStats where

import Data.Aeson
import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.IncentiveJourneyMilestone
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data IncentiveJourneyStats = IncentiveJourneyStats
  { conditionOperator :: Kernel.Prelude.Maybe Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currentValue :: Kernel.Prelude.Int,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats,
    journeyId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney,
    milestoneId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    periodKey :: Kernel.Prelude.Text,
    rewardType :: Domain.Types.IncentiveJourneyMilestone.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data JourneyMilestoneStatus = NotStarted | InProgress | Completed | Rewarded deriving (Generic, (Show), (Read), (Eq), (Ord), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''JourneyMilestoneStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''JourneyMilestoneStatus))
