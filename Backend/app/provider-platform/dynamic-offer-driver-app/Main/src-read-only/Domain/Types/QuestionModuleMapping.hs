{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.QuestionModuleMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.LmsModule
import qualified Lib.DriverCoins.Types
import qualified Tools.Beam.UtilsTH



data QuestionModuleMapping
    = QuestionModuleMapping {moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
                             questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping,
                             quizCoinFunction :: Kernel.Prelude.Maybe Lib.DriverCoins.Types.DriverCoinsFunctionType,
                             createdAt :: Kernel.Prelude.UTCTime,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



