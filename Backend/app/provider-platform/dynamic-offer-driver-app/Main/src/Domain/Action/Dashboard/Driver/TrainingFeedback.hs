{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.TrainingFeedback
  ( listTrainingModuleFeedback,
    getTrainingModuleFeedbackDetail,
    getTrainingFeedbackSummary,
    TrainingModuleListResponse (..),
    TrainingModuleEntry (..),
    TrainingModuleFeedbackDetailResponse (..),
    FeedbackEntry (..),
    TrainingFeedbackSummaryResponse (..),
    RatingDistribution (..),
    ModuleSummary (..),
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common

-- ============================================
-- Types
-- ============================================

data TrainingModuleListResponse = TrainingModuleListResponse
  { modules :: [TrainingModuleEntry],
    totalCount :: Int,
    summary :: TrainingModuleListSummary
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TrainingModuleListSummary = TrainingModuleListSummary
  { overallAvgRating :: Double,
    totalFeedbackCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TrainingModuleEntry = TrainingModuleEntry
  { moduleId :: Text,
    moduleName :: Text,
    avgRating :: Double,
    totalResponses :: Int,
    promoterCount :: Int,
    detractorCount :: Int,
    npsScore :: Double,
    lastFeedbackAt :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TrainingModuleFeedbackDetailResponse = TrainingModuleFeedbackDetailResponse
  { feedback :: [FeedbackEntry],
    totalCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackEntry = FeedbackEntry
  { feedbackId :: Text,
    driverId :: Text,
    driverName :: Text,
    driverPhone :: Text,
    rating :: Int,
    feedbackText :: Maybe Text,
    completedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TrainingFeedbackSummaryResponse = TrainingFeedbackSummaryResponse
  { overallAvgRating :: Double,
    totalFeedbackCount :: Int,
    npsScore :: Double,
    feedbackRate :: Double,
    ratingDistribution :: RatingDistribution,
    topModule :: Maybe ModuleSummary,
    bottomModule :: Maybe ModuleSummary
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RatingDistribution = RatingDistribution
  { rating1 :: Int,
    rating2 :: Int,
    rating3 :: Int,
    rating4 :: Int,
    rating5 :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ModuleSummary = ModuleSummary
  { moduleId :: Text,
    moduleName :: Text,
    avgRating :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- ============================================
-- Handlers
-- ============================================

-- | List training modules with aggregated feedback ratings.
-- Reads from the training_module_feedback_aggregated materialized view.
listTrainingModuleFeedback ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->  -- from date
  Maybe Text ->  -- to date
  Maybe Text ->  -- sortBy
  Maybe Text ->  -- sortOrder
  Maybe Int ->   -- limit
  Maybe Int ->   -- offset
  Flow TrainingModuleListResponse
listTrainingModuleFeedback _merchantShortId _merchantOpCityId _mbFrom _mbTo _mbSortBy _mbSortOrder mbLimit mbOffset = do
  let _limit = fromMaybe 20 mbLimit
      _offset = fromMaybe 0 mbOffset
  -- TODO: Query training_module_feedback_aggregated materialized view
  -- filtered by merchantId and cityId, with optional date range filtering.
  -- For now, return empty response as the query implementation depends on
  -- the Beam schema generation from the storage YAML spec.
  logInfo "TrainingFeedback: listTrainingModuleFeedback called"
  pure $
    TrainingModuleListResponse
      { modules = [],
        totalCount = 0,
        summary =
          TrainingModuleListSummary
            { overallAvgRating = 0.0,
              totalFeedbackCount = 0
            }
      }

-- | Get individual feedback entries for a specific training module.
getTrainingModuleFeedbackDetail ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->         -- moduleId
  Maybe Text ->   -- from date
  Maybe Text ->   -- to date
  Maybe Int ->    -- rating filter
  Maybe Int ->    -- limit
  Maybe Int ->    -- offset
  Flow TrainingModuleFeedbackDetailResponse
getTrainingModuleFeedbackDetail _merchantShortId _merchantOpCityId _moduleId _mbFrom _mbTo _mbRating mbLimit mbOffset = do
  let _limit = fromMaybe 20 mbLimit
      _offset = fromMaybe 0 mbOffset
  -- TODO: Query training_module_feedback table joined with person table
  -- for driver name/phone, filtered by moduleId, merchantId, cityId.
  logInfo "TrainingFeedback: getTrainingModuleFeedbackDetail called"
  pure $
    TrainingModuleFeedbackDetailResponse
      { feedback = [],
        totalCount = 0
      }

-- | Get summary statistics for training feedback (KPIs).
getTrainingFeedbackSummary ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->  -- from date
  Maybe Text ->  -- to date
  Flow TrainingFeedbackSummaryResponse
getTrainingFeedbackSummary _merchantShortId _merchantOpCityId _mbFrom _mbTo = do
  -- TODO: Compute aggregated summary from training_module_feedback table:
  -- - Overall average rating
  -- - Total feedback count
  -- - NPS score: ((promoters - detractors) / total) * 100
  -- - Feedback rate: (feedback count / completion count) * 100
  -- - Rating distribution counts
  -- - Top and bottom modules by avg rating
  logInfo "TrainingFeedback: getTrainingFeedbackSummary called"
  pure $
    TrainingFeedbackSummaryResponse
      { overallAvgRating = 0.0,
        totalFeedbackCount = 0,
        npsScore = 0.0,
        feedbackRate = 0.0,
        ratingDistribution =
          RatingDistribution
            { rating1 = 0,
              rating2 = 0,
              rating3 = 0,
              rating4 = 0,
              rating5 = 0
            },
        topModule = Nothing,
        bottomModule = Nothing
      }
