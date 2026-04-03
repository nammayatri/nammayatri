{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.LmsVideoTranslation where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.ReelsData
import qualified Kernel.External.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.LmsModuleVideoInformation
import qualified Tools.Beam.UtilsTH



data LmsVideoTranslation
    = LmsVideoTranslation {bottomButtonConfig :: [Domain.Types.ReelsData.ReelRowButtonConfig],
                           completedThresholdInPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                           completedWatchCount :: Kernel.Prelude.Int,
                           description :: Kernel.Prelude.Text,
                           duration :: Kernel.Prelude.Int,
                           language :: Kernel.External.Types.Language,
                           sideButtonConfig :: [Domain.Types.ReelsData.ReelRowButtonConfig],
                           startThresholdInPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                           thresholdEnabled :: Kernel.Prelude.Bool,
                           thumbnailImage :: Kernel.Prelude.Text,
                           title :: Kernel.Prelude.Text,
                           url :: Kernel.Prelude.Text,
                           useMerchantOperatingCityDefaultLanguageVideoUrl :: Kernel.Prelude.Bool,
                           videoId :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation,
                           viewCount :: Kernel.Prelude.Int,
                           ytVideoId :: Kernel.Prelude.Text,
                           createdAt :: Kernel.Prelude.UTCTime,
                           updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



