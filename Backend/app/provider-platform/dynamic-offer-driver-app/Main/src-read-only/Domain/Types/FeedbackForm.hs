{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FeedbackForm where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FeedbackForm = FeedbackForm
  { answer :: [Kernel.Prelude.Text],
    answerType :: Domain.Types.FeedbackForm.AnswerType,
    badges :: Kernel.Prelude.Maybe [Domain.Types.FeedbackForm.BadgeDetail],
    categoryName :: Domain.Types.FeedbackForm.Category,
    id :: Kernel.Types.Id.Id Domain.Types.FeedbackForm.FeedbackFormItem,
    question :: Kernel.Prelude.Text,
    questionTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data AnswerType = Text | Checkbox | Radio deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data BadgeDetail = BadgeDetail
  { contentWithTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    key :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    sendPN :: Kernel.Prelude.Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data BadgeItem = BadgeItem {key :: Kernel.Prelude.Text, translations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation]} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Category = RIDE | DRIVER | VEHICLE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackAnswer = FeedbackAnswer {answer :: [Kernel.Prelude.Text], questionId :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackFormAPIEntity = FeedbackFormAPIEntity {questions :: [Domain.Types.FeedbackForm.FeedbackQuestion], rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackFormItem = FeedbackFormItem
  { answer :: [Kernel.Prelude.Text],
    answerType :: Domain.Types.FeedbackForm.AnswerType,
    badges :: Kernel.Prelude.Maybe [Domain.Types.FeedbackForm.BadgeDetail],
    id :: Kernel.Types.Id.Id Domain.Types.FeedbackForm.FeedbackFormItem,
    question :: Kernel.Prelude.Text,
    questionTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

newtype FeedbackFormList = FeedbackFormList {_data :: [Domain.Types.FeedbackForm.FeedbackFormAPIEntity]} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackFormReq = FeedbackFormReq {feedback :: Kernel.Prelude.Maybe [Domain.Types.FeedbackForm.FeedbackAnswer], rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackQuestion = FeedbackQuestion
  { badges :: [Domain.Types.FeedbackForm.BadgeItem],
    question :: Kernel.Prelude.Text,
    questionId :: Kernel.Prelude.Text,
    questionTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AnswerType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Category)
