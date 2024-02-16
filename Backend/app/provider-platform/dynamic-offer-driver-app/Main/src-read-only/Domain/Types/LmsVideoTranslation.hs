{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LmsVideoTranslation where

import qualified Domain.Types.LmsModuleVideoInformation
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LmsVideoTranslation = LmsVideoTranslation
  { bottomButtonConfig :: [Domain.Types.LmsVideoTranslation.ReelRowButtonConfig],
    completedThresholdInPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    completedWatchCount :: Kernel.Prelude.Int,
    description :: Kernel.Prelude.Text,
    duration :: Kernel.Prelude.Int,
    language :: Kernel.External.Types.Language,
    sideButtonConfig :: [Domain.Types.LmsVideoTranslation.ReelRowButtonConfig],
    startThresholdInPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    thresholdEnabled :: Kernel.Prelude.Bool,
    thumbnailImage :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    videoId :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation,
    viewCount :: Kernel.Prelude.Int,
    ytVideoId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ReelButtonConfig = ReelButtonConfig
  { actions :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    activeIndex :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    activeIndexHeight :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    activeIndexWidth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    buttonColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cornerRadius :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    inActiveIndex :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    inActiveIndexHeight :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    inActiveIndexWidth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    prefixImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    prefixImageHeight :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    prefixImageWidth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    shareLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shareText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    suffixImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    suffixImageHeight :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    suffixImageWidth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    text :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    textColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    textSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data ReelRowButtonConfig = ReelRowButtonConfig
  { row :: [Domain.Types.LmsVideoTranslation.ReelButtonConfig]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReelButtonConfig)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReelRowButtonConfig)
