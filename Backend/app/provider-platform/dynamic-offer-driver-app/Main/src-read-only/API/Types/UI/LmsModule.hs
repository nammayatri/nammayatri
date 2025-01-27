{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.LmsModule where

import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.DriverModuleCompletion
import qualified Domain.Types.LmsCertificate
import qualified Domain.Types.LmsEnumTypes
import qualified Domain.Types.LmsModule
import qualified Domain.Types.LmsModuleVideoInformation
import qualified Domain.Types.Person
import qualified Domain.Types.QuestionInformation
import qualified Domain.Types.QuestionModuleMapping
import qualified Domain.Types.ReelsData
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data BonusRes = BonusRes {coins :: Data.Maybe.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CertificateInfo = CertificateInfo
  { certificateCourseName :: Data.Text.Text,
    certificateId :: Kernel.Types.Id.Id Domain.Types.LmsCertificate.LmsCertificate,
    certificateOwnerName :: Data.Text.Text,
    completedAt :: Data.Maybe.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsCertificateRes = LmsCertificateRes {certificateInfo :: Data.Maybe.Maybe CertificateInfo}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsEntityCompletionStatus
  = ENTITY_COMPLETED
  | ENTITY_INCOMPLETE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsGetModuleRes = LmsGetModuleRes {completed :: [LmsModuleRes], remaining :: [LmsModuleRes]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsGetQuizRes = LmsGetQuizRes {questions :: [LmsQuestionRes], selectedModuleInfo :: LmsTranslatedModuleInfoRes}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsGetVideosRes = LmsGetVideosRes {completed :: [LmsVideoRes], pending :: [LmsVideoRes], quizEnabled :: Kernel.Prelude.Bool, quizStatus :: LmsEntityCompletionStatus, selectedModuleInfo :: LmsTranslatedModuleInfoRes}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsModuleRes = LmsModuleRes
  { bonusCoins :: Data.Maybe.Maybe Kernel.Prelude.Int,
    bonusEarnedVal :: Data.Maybe.Maybe Kernel.Prelude.Int,
    category :: Domain.Types.LmsModule.LmsCategory,
    certificationEnabled :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    completedAt :: Data.Maybe.Maybe Kernel.Prelude.UTCTime,
    description :: Data.Text.Text,
    duration :: Kernel.Prelude.Int,
    languagesAvailableForQuiz :: [Kernel.External.Types.Language],
    languagesAvailableForVideos :: [Kernel.External.Types.Language],
    moduleAlreadyCompleted :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    moduleCompletionCriteria :: Domain.Types.LmsModule.ModuleCompletionCriteria,
    moduleCompletionStatus :: Domain.Types.DriverModuleCompletion.ModuleCompletionStatus,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    moduleSection :: Data.Maybe.Maybe Domain.Types.LmsModule.ModuleSection,
    name :: Data.Text.Text,
    noOfVideos :: Kernel.Prelude.Int,
    noOfVideosCompleted :: Kernel.Prelude.Int,
    rank :: Kernel.Prelude.Int,
    thumbnailImage :: Data.Text.Text,
    totalQuizCoins :: Data.Maybe.Maybe Kernel.Prelude.Int,
    variant :: Data.Maybe.Maybe Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsQuestionRes = LmsQuestionRes
  { language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    options :: QuizOptions,
    previousHistory :: Data.Maybe.Maybe LmsQuizHistory,
    question :: Domain.Types.LmsEnumTypes.QuizQuestion,
    questionCoins :: Data.Maybe.Maybe Kernel.Prelude.Int,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsQuestionStatus
  = CORRECT
  | INCORRECT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsQuizHistory = LmsQuizHistory {attemptNumber :: Kernel.Prelude.Int, coinsEarned :: Data.Maybe.Maybe Kernel.Prelude.Int, selectedOptions :: [Data.Text.Text], status :: LmsQuestionStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsTranslatedModuleInfoRes = LmsTranslatedModuleInfoRes
  { category :: Domain.Types.LmsModule.LmsCategory,
    description :: Data.Text.Text,
    duration :: Kernel.Prelude.Int,
    languagesAvailableForQuiz :: [Kernel.External.Types.Language],
    languagesAvailableForVideos :: [Kernel.External.Types.Language],
    moduleCompletionCriteria :: Domain.Types.LmsModule.ModuleCompletionCriteria,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    name :: Data.Text.Text,
    noOfVideos :: Kernel.Prelude.Int,
    rank :: Kernel.Prelude.Int,
    thumbnailImage :: Data.Text.Text,
    variant :: Data.Maybe.Maybe Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsVideoRes = LmsVideoRes
  { attemptNumber :: Kernel.Prelude.Int,
    bottomButtonConfig :: [[Domain.Types.ReelsData.ReelButtonConfig]],
    completedAt :: Kernel.Prelude.UTCTime,
    completedThresholdInPercentage :: Data.Maybe.Maybe Kernel.Prelude.Int,
    completedWatchCount :: Kernel.Prelude.Int,
    description :: Data.Text.Text,
    duration :: Kernel.Prelude.Int,
    language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    rank :: Kernel.Prelude.Int,
    sideButtonConfig :: [[Domain.Types.ReelsData.ReelButtonConfig]],
    startThresholdInPercentage :: Data.Maybe.Maybe Kernel.Prelude.Int,
    thresholdEnabled :: Kernel.Prelude.Bool,
    thumbnailImage :: Data.Text.Text,
    title :: Data.Text.Text,
    url :: Data.Text.Text,
    videoCompletionStatus :: LmsEntityCompletionStatus,
    videoId :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation,
    viewCount :: Kernel.Prelude.Int,
    ytVideoId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Options = Options {options :: [Domain.Types.QuestionInformation.OptionEntity]}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuestionConfirmReq = QuestionConfirmReq
  { language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping,
    selectedOption :: SelectedOption
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuestionConfirmRes = QuestionConfirmRes
  { bonusEarned :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    coinsEarned :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    validation :: QuestionValidation,
    validationRes :: SelectedOptionValidation
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuestionValidation
  = CORRECT_ANSWER
  | INCORRECT_ANSWER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuizOptions
  = SingleSelect Options
  | MultiSelect Options
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectedOption
  = SingleSelectedOption Data.Text.Text
  | MultiSelectedOption [Data.Text.Text]
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectedOptionValidation
  = SingleSelectedOptionValidation ValidationResult
  | MultiSelectedOptionValidation [ValidationResult]
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ValidationResult = ValidationResult {id :: Data.Text.Text, isCorrect :: Kernel.Prelude.Bool}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VideoUpdateAPIReq = VideoUpdateAPIReq
  { language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    videoId :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
