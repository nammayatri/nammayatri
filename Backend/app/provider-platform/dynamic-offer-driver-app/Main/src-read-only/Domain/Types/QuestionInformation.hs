{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.QuestionInformation where

import Data.Aeson
import qualified Domain.Types.LmsEnumTypes
import qualified Domain.Types.QuestionModuleMapping
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data QuestionInformation = QuestionInformation
  { language :: Kernel.External.Types.Language,
    options :: [Domain.Types.QuestionInformation.OptionEntity],
    question :: Domain.Types.LmsEnumTypes.QuizQuestion,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping,
    questionType :: Domain.Types.QuestionInformation.QuizQuestionType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OptionEntity = OptionEntity {isCorrect :: Kernel.Prelude.Bool, option :: Domain.Types.QuestionInformation.SingleOption, optionId :: Kernel.Types.Id.Id Domain.Types.QuestionInformation.OptionEntity}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read, Eq, Ord)

data QuizQuestionType = SingleSelect | MultiSelect deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SingleOption
  = TextOption Kernel.Prelude.Text
  | SingleLineImage Kernel.Prelude.Text Kernel.Prelude.Int Kernel.Prelude.Int
  | TwoColumnImage Kernel.Prelude.Text Kernel.Prelude.Int Kernel.Prelude.Int
  | TwoColumnOption Kernel.Prelude.Text
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''QuizQuestionType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SingleOption)
