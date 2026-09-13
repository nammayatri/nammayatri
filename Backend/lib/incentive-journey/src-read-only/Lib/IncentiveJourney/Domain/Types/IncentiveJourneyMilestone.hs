{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone where

import Data.Aeson
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Kernel.Utils.TH
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Tools.Beam.UtilsTH

data IncentiveJourneyMilestone = IncentiveJourneyMilestone
  { areaType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneAreaType,
    conditionOperator :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    rewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rewardType :: Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    specialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MilestoneAreaType = Default | Pickup | Drop | PickupDrop deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

data MilestoneConditionOperator = GTE | GT | EQ | LTE | LT deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

data MilestoneConditionType
  = RideCompleted
  | Earnings
  | Distance
  | RideDuration
  | BookingTicket
  deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MilestoneConditionType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MilestoneConditionType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MilestoneConditionOperator)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MilestoneConditionOperator)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MilestoneAreaType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MilestoneAreaType)
