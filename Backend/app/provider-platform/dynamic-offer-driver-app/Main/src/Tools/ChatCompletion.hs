module Tools.ChatCompletion (getChatCompletion) where

import ChatCompletion.Interface as CI
import ChatCompletion.Interface.Types as CIT
import ChatCompletion.Types as CT
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DOSC
import Domain.Types.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import Tools.Error

getChatCompletion :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> CIT.GeneralChatCompletionReq -> m CIT.GeneralChatCompletionResp
getChatCompletion merchantId merchantOpCityId req = do
  runWithServiceConfig CI.chatCompletion (.llmChatCompletion) merchantId merchantOpCityId req

runWithServiceConfig ::
  ServiceFlow m r =>
  (CIT.LLMChatCompletionServiceConfig -> CIT.GeneralChatCompletionReq -> m CIT.GeneralChatCompletionResp) ->
  (MerchantServiceUsageConfig -> LLMChatCompletionService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  CIT.GeneralChatCompletionReq ->
  m CIT.GeneralChatCompletionResp
runWithServiceConfig func getCfg _merchantId merchantOpCityId req = do
  orgLLMChatCompletionConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  orgLLMChatCompletionServiceConfig <-
    QOMSC.findByServiceAndCity (DOSC.LLMChatCompletionService $ getCfg orgLLMChatCompletionConfig) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "LLMChatCompletion" (show $ getCfg orgLLMChatCompletionConfig))
  case orgLLMChatCompletionServiceConfig.serviceConfig of
    DOSC.LLMChatCompletionServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
