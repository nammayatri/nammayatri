{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.LlmPrompt where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.LlmPrompt
import qualified Domain.Types.Extra.MerchantServiceConfig
import qualified Database.Beam as B



data LlmPromptT f
    = LlmPromptT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                  merchantId :: (B.C f Kernel.Prelude.Text),
                  merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                  promptKey :: (B.C f Domain.Types.LlmPrompt.PromptKey),
                  promptTemplate :: (B.C f Kernel.Prelude.Text),
                  serviceName :: (B.C f Domain.Types.Extra.MerchantServiceConfig.ServiceName),
                  updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                  useCase :: (B.C f Domain.Types.LlmPrompt.UseCase)}
    deriving (Generic, B.Beamable)
instance B.Table LlmPromptT
    where data PrimaryKey LlmPromptT f
              = LlmPromptId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.LlmPrompt.PromptKey) (B.C f Domain.Types.Extra.MerchantServiceConfig.ServiceName) (B.C f Domain.Types.LlmPrompt.UseCase)
              deriving (Generic, B.Beamable)
          primaryKey = LlmPromptId <$> merchantOperatingCityId <*> promptKey <*> serviceName <*> useCase
type LlmPrompt = LlmPromptT Identity

$(enableKVPG (''LlmPromptT) [('merchantOperatingCityId), ('promptKey), ('serviceName), ('useCase)] [])

$(mkTableInstances (''LlmPromptT) "llm_prompt")

