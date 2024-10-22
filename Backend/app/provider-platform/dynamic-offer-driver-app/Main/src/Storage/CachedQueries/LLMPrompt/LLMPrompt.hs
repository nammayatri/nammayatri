{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.LLMPrompt.LLMPrompt
  ( create,
    findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Extra.MerchantServiceConfig
import Domain.Types.LlmPrompt
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.LlmPrompt as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LlmPrompt -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [LlmPrompt]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return $ fmap (coerce @(LlmPromptD 'Unsafe) @LlmPrompt) a
    Nothing -> cacheLlmPromptForCity id /=<< Queries.findAllByMerchantOpCityId id

cacheLlmPromptForCity :: CacheFlow m r => Id MerchantOperatingCity -> [LlmPrompt] -> m ()
cacheLlmPromptForCity merchantOperatingCityId llmPrompts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (fmap (coerce @LlmPrompt @(LlmPromptD 'Unsafe)) llmPrompts) expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:LlmPrompt:MerchantOperatingCityId-" <> id.getId

findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> ServiceName -> UseCase -> PromptKey -> m (Maybe LlmPrompt)
findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey opCityId serviceName useCase promptKey =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKeyKey opCityId serviceName useCase promptKey) >>= \case
    Just a -> return . Just $ coerce @(LlmPromptD 'Unsafe) @LlmPrompt a
    Nothing -> flip whenJust (cacheLlmPrompt opCityId) /=<< Queries.findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey opCityId serviceName useCase promptKey

makeMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKeyKey :: Id MerchantOperatingCity -> ServiceName -> UseCase -> PromptKey -> Text
makeMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKeyKey opCityId serviceName useCase promptKey = "driver-offer:CachedQueries:LlmPrompt:MerchantOperatingCityId-" <> opCityId.getId <> ":ServiceName-" <> show serviceName <> ":UseCase-" <> show useCase <> ":PromptKey-" <> show promptKey

cacheLlmPrompt :: CacheFlow m r => Id MerchantOperatingCity -> LlmPrompt -> m ()
cacheLlmPrompt merchantOperatingCity llmPrompt = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKeyKey merchantOperatingCity (llmPrompt.serviceName) (llmPrompt.useCase) (llmPrompt.promptKey)
  Hedis.withCrossAppRedis $ Hedis.setExp idKey (coerce @LlmPrompt @(LlmPromptD 'Unsafe) llmPrompt) expTime
