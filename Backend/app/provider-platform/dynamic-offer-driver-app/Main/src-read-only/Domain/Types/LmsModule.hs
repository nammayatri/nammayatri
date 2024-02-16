{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LmsModule where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LmsModule = LmsModule
  { category :: Domain.Types.LmsModule.LmsCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    duration :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    languagesAvailableForQuiz :: [Kernel.External.Types.Language],
    languagesAvailableForVideos :: [Kernel.External.Types.Language],
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    moduleCompletionCriteria :: Domain.Types.LmsModule.ModuleCompletionCriteria,
    noOfVideos :: Kernel.Prelude.Int,
    rank :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    variant :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant.Variant,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data LmsCategory = Safety | Financial | Training
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ModuleCompletionCriteria = ONLY_VIDEOS | VIDEOS_AND_QUIZ Kernel.Prelude.Int
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''LmsCategory)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ModuleCompletionCriteria)
