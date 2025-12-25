module ChatCompletion.Interface.AzureOpenAI (azureOpenAIChatCompletion) where

import ChatCompletion.AzureOpenAI.API as CAA
import ChatCompletion.AzureOpenAI.Config as CAC
import ChatCompletion.AzureOpenAI.Types as CAT
import ChatCompletion.Interface.Types as CIT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

azureOpenAIChatCompletion :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => CAC.AzureOpenAICfg -> CIT.GeneralChatCompletionReq -> m CIT.GeneralChatCompletionResp
azureOpenAIChatCompletion cfg req = do
  let chatCompletionUrl = cfg.azureOpenAIChatCompletionUrl
      apiVersion = cfg.azureApiVersion
  apiKey <- decrypt cfg.azureApiKey
  let azureOAIccReqMsgs = fmap toAzureOAIccReq req.genMessages
      azureOAIccReq = CAT.ChatCompletionReq {messages = azureOAIccReqMsgs}
  azureCCresp <- CAA.chatCompletion chatCompletionUrl apiVersion apiKey azureOAIccReq
  let choice = last $ azureCCresp.choices
      respContent = choice.message.content
      respRole = choice.message.role
      genMessage = CIT.GeneralChatCompletionMessage {genContent = respContent, genRole = respRole}
      generalCCResp = CIT.GeneralChatCompletionResp {genMessage = genMessage}
  pure generalCCResp

toAzureOAIccReq :: CIT.GeneralChatCompletionMessage -> CAT.Message
toAzureOAIccReq genMsg = CAT.Message {role = genMsg.genRole, content = genMsg.genContent}
