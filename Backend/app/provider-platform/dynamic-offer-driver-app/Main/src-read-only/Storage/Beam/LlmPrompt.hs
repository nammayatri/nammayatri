{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LlmPrompt where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.MerchantServiceConfig
import qualified Domain.Types.LlmPrompt
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data LlmPromptT f = LlmPromptT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    promptKey :: B.C f Domain.Types.LlmPrompt.PromptKey,
    promptTemplate :: B.C f Kernel.Prelude.Text,
    serviceName :: B.C f Domain.Types.Extra.MerchantServiceConfig.ServiceName,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    useCase :: B.C f Domain.Types.LlmPrompt.UseCase
  }
  deriving (Generic, B.Beamable)

instance B.Table LlmPromptT where
  data PrimaryKey LlmPromptT f
    = LlmPromptId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.LlmPrompt.PromptKey) (B.C f Domain.Types.Extra.MerchantServiceConfig.ServiceName) (B.C f Domain.Types.LlmPrompt.UseCase)
    deriving (Generic, B.Beamable)
  primaryKey = LlmPromptId <$> merchantOperatingCityId <*> promptKey <*> serviceName <*> useCase

type LlmPrompt = LlmPromptT Identity

$(enableKVPG ''LlmPromptT ['merchantOperatingCityId, 'promptKey, 'serviceName, 'useCase] [])

$(mkTableInstances ''LlmPromptT "llm_prompt")
