{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.QuestionInformation where

import qualified Domain.Types.QuestionModuleMapping
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data QuestionInformation = QuestionInformation
  { language :: Kernel.External.Types.Language,
    options :: Domain.Types.QuestionInformation.QuizOptions,
    question :: Domain.Types.QuestionInformation.QuizQuestion,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OptionEntity = OptionEntity
  { isCorrect :: Kernel.Prelude.Bool,
    option :: Domain.Types.QuestionInformation.SingleOption,
    optionId :: Kernel.Types.Id.Id Domain.Types.QuestionInformation.OptionEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read, Eq, Ord)

data Options = Options
  { options :: [Domain.Types.QuestionInformation.OptionEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read, Eq, Ord)

data QuizOptions = SingleSelect Domain.Types.QuestionInformation.Options | MultiSelect Domain.Types.QuestionInformation.Options
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data QuizQuestion = TextQuestion Kernel.Prelude.Text | ImageQuestion Kernel.Prelude.Text Kernel.Prelude.Text Kernel.Prelude.Int Kernel.Prelude.Int
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SingleOption = TextOption Kernel.Prelude.Text | SingleLineImage Kernel.Prelude.Text Kernel.Prelude.Int Kernel.Prelude.Int | TwoColumnImage Kernel.Prelude.Text Kernel.Prelude.Int Kernel.Prelude.Int | TwoColumnOption Kernel.Prelude.Text
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''QuizOptions)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''QuizQuestion)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SingleOption)
