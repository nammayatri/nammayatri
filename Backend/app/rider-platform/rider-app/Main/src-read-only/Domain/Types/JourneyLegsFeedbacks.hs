{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyLegsFeedbacks where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data JourneyLegsFeedbacks = JourneyLegsFeedbacks
  { feedbackData :: Kernel.Prelude.Maybe Domain.Types.JourneyLegsFeedbacks.JourneyLegFeedbackData,
    isExperienceGood :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legOrder :: Kernel.Prelude.Int,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    travelMode :: Kernel.Prelude.Maybe Domain.Types.Common.MultimodalTravelMode,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BusFeedbackData = BusFeedbackData {feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data JourneyLegFeedbackData
  = Taxi Domain.Types.JourneyLegsFeedbacks.TaxiFeedbackData
  | Bus Domain.Types.JourneyLegsFeedbacks.BusFeedbackData
  | Walk Domain.Types.JourneyLegsFeedbacks.WalkFeedbackData
  | Subway Domain.Types.JourneyLegsFeedbacks.SubwayFeedbackData
  | Metro Domain.Types.JourneyLegsFeedbacks.MetroFeedbackData
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data MetroFeedbackData = MetroFeedbackData {feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SubwayFeedbackData = SubwayFeedbackData {feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data TaxiFeedbackData = TaxiFeedbackData
  { feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mbAudio :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shouldFavDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    wasOfferedAssistance :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    wasRideSafe :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WalkFeedbackData = WalkFeedbackData {feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
