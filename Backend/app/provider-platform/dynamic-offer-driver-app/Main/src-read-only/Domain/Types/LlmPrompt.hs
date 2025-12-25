{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LlmPrompt where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Extra.MerchantServiceConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LlmPromptD (s :: UsageSafety) = LlmPrompt
  { createdAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    promptKey :: Domain.Types.LlmPrompt.PromptKey,
    promptTemplate :: Kernel.Prelude.Text,
    serviceName :: Domain.Types.Extra.MerchantServiceConfig.ServiceName,
    updatedAt :: Kernel.Prelude.UTCTime,
    useCase :: Domain.Types.LlmPrompt.UseCase
  }
  deriving (Generic, Show)

data PromptKey = AzureOpenAI_DriverProfileGen_1 | AzureOpenAI_DLExtraction_1 | AzureOpenAI_RCExtraction_1 deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data UseCase = DriverProfileGen | DriverSupport | DLExtraction | RCExtraction deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type LlmPrompt = LlmPromptD 'Safe

instance FromJSON (LlmPromptD 'Unsafe)

instance ToJSON (LlmPromptD 'Unsafe)

instance FromJSON (LlmPromptD 'Safe)

instance ToJSON (LlmPromptD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PromptKey)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''UseCase)
