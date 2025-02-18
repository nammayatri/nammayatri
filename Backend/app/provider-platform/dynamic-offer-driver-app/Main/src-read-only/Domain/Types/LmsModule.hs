{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LmsModule where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleVariant
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.DriverCoins.Types
import qualified Tools.Beam.UtilsTH

data LmsModule = LmsModule
  { bonusCoinEventFunction :: Kernel.Prelude.Maybe Lib.DriverCoins.Types.DriverCoinsFunctionType,
    category :: Domain.Types.LmsModule.LmsCategory,
    certificationEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    duration :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    languagesAvailableForQuiz :: [Kernel.External.Types.Language],
    languagesAvailableForVideos :: [Kernel.External.Types.Language],
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    moduleCompletionCriteria :: Domain.Types.LmsModule.ModuleCompletionCriteria,
    moduleExpiryConfig :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    moduleNameForCertificate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    moduleSection :: Kernel.Prelude.Maybe Domain.Types.LmsModule.ModuleSection,
    noOfVideos :: Kernel.Prelude.Int,
    rank :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    variant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data LmsCategory = Safety | Financial | Training | DriverSafetyScore deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ModuleCompletionCriteria = ONLY_VIDEOS | VIDEOS_AND_QUIZ Kernel.Prelude.Int deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ModuleSection = BENEFITS | DRIVER_SAFETY_SCORE deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''LmsCategory)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ModuleCompletionCriteria)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ModuleSection)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''ModuleSection)
