module Tools.ChatCompletion
  ( getChatCompletion,
  )
where

import qualified ChatCompletion.Interface as CI
import qualified ChatCompletion.Interface.Types as CIT
import qualified ChatCompletion.Types as CT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

getChatCompletion ::
  (ServiceFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  CT.LLMChatCompletionService ->
  CIT.GeneralChatCompletionReq ->
  m CIT.GeneralChatCompletionResp
getChatCompletion merchantId merchantOpCityId service req = do
  let serviceName = DMSC.LLMChatCompletionService service
  msc <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName}) (Just (maybeToList <$> CQMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId serviceName))
      >>= fromMaybeM (InternalError $ "LLMChatCompletion " <> show service <> " service config not found for merchantOpCityId: " <> merchantOpCityId.getId)
  case msc.serviceConfig of
    DMSC.LLMChatCompletionServiceConfig cfg -> CI.chatCompletion cfg req
    _ -> throwError $ InternalError $ "Unexpected service config shape for LLMChatCompletion " <> show service
