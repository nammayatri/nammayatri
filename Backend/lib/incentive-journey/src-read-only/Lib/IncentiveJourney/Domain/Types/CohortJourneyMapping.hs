{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Tools.Beam.UtilsTH

data CohortJourneyMapping = CohortJourneyMapping
  { campaignUserLinkType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CampaignUserLinkType,
    cohortId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails,
    controlFeatures :: Kernel.Prelude.Maybe Data.Aeson.Value,
    createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    maxWaiveOffCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity,
    parentId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping),
    startDate :: Kernel.Prelude.UTCTime,
    streakEndRewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardMetadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    streakEndRewardType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    streakEndRewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakRange :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CampaignUserLinkType = Assigned | OptIn deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''CampaignUserLinkType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''CampaignUserLinkType)
