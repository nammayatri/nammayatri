{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ReelsData where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ReelsData = ReelsData
  { bottomButtonConfig :: [Domain.Types.ReelsData.ReelRowButtonConfig],
    carouselBigImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    carouselSmallImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    carouselTextColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    carouselTextString :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ReelsData.ReelsData,
    language :: Kernel.External.Types.Language,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rank :: Kernel.Prelude.Int,
    reelKey :: Kernel.Prelude.Text,
    shareLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sideButtonConfig :: [Domain.Types.ReelsData.ReelRowButtonConfig],
    thresholdConfig :: Kernel.Prelude.Maybe Domain.Types.ReelsData.ReelVideoThresholdConfig,
    thumbnailImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    videoUrl :: Kernel.Prelude.Text,
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

data ReelRowButtonConfig = ReelRowButtonConfig {row :: [Domain.Types.ReelsData.ReelButtonConfig]} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data ReelVideoThresholdConfig = ReelVideoThresholdConfig
  { endThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isEndThresholdEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isStartThresholdEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isThresholdEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    sendCallbackAfterEverySecondEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    startThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReelButtonConfig)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReelRowButtonConfig)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReelVideoThresholdConfig)
