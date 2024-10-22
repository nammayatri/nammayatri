{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LlmPrompt where

import qualified Domain.Types.Extra.MerchantServiceConfig
import qualified Domain.Types.LlmPrompt
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LlmPrompt as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LlmPrompt.LlmPrompt -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.LlmPrompt.LlmPrompt] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.LlmPrompt.LlmPrompt])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Extra.MerchantServiceConfig.ServiceName -> Domain.Types.LlmPrompt.UseCase -> Domain.Types.LlmPrompt.PromptKey -> m (Maybe Domain.Types.LlmPrompt.LlmPrompt))
findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey merchantOperatingCityId serviceName useCase promptKey = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.useCase $ Se.Eq useCase,
          Se.Is Beam.promptKey $ Se.Eq promptKey
        ]
    ]

instance FromTType' Beam.LlmPrompt Domain.Types.LlmPrompt.LlmPrompt where
  fromTType' (Beam.LlmPromptT {..}) = do
    pure $
      Just
        Domain.Types.LlmPrompt.LlmPrompt
          { createdAt = createdAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            promptKey = promptKey,
            promptTemplate = promptTemplate,
            serviceName = serviceName,
            updatedAt = updatedAt,
            useCase = useCase
          }

instance ToTType' Beam.LlmPrompt Domain.Types.LlmPrompt.LlmPrompt where
  toTType' (Domain.Types.LlmPrompt.LlmPrompt {..}) = do
    Beam.LlmPromptT
      { Beam.createdAt = createdAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.promptKey = promptKey,
        Beam.promptTemplate = promptTemplate,
        Beam.serviceName = serviceName,
        Beam.updatedAt = updatedAt,
        Beam.useCase = useCase
      }
