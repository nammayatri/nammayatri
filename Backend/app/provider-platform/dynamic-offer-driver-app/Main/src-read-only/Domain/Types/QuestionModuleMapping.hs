{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.QuestionModuleMapping where

import Data.Aeson
import qualified Domain.Types.LmsModule
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.DriverCoins.Types
import qualified Tools.Beam.UtilsTH

data QuestionModuleMapping = QuestionModuleMapping
  { moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping,
    quizCoinFunction :: Kernel.Prelude.Maybe Lib.DriverCoins.Types.DriverCoinsFunctionType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
