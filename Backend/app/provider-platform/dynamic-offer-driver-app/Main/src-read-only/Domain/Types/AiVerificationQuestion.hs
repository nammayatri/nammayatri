{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AiVerificationQuestion where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AiVerificationQuestion = AiVerificationQuestion
  { enabled :: Kernel.Prelude.Bool,
    entityType :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.AiVerificationQuestion.AiVerificationQuestion,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    options :: Kernel.Prelude.Maybe [Domain.Types.AiVerificationQuestion.AiQuestionOption],
    question :: Kernel.Prelude.Text,
    questionLabel :: Kernel.Prelude.Text,
    questionType :: Domain.Types.AiVerificationQuestion.AiQuestionType,
    threshold :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq), (FromJSON), (ToJSON))

data AiQuestionOption = AiQuestionOption {key :: Kernel.Prelude.Text, text :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data AiQuestionType = NOUL | CHOICE | SCORE deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''AiQuestionType))
