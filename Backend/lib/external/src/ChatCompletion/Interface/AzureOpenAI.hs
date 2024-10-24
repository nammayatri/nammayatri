module ChatCompletion.Interface.AzureOpenAI (azureOpenAIChatCompletion) where

import ChatCompletion.AzureOpenAI.API as CAA
import ChatCompletion.AzureOpenAI.Config as CAC
import ChatCompletion.AzureOpenAI.Types as CAT
import Kernel.External.Encryption
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

azureOpenAIChatCompletion :: (EncFlow m r, CoreMetrics m, Log m) => CAC.AzureOpenAICfg -> CAT.ChatCompletionReq -> m CAT.ChatCompletionResponse
azureOpenAIChatCompletion cfg req = do
  let chatCompletionUrl = cfg.azureOpenAIChatCompletionUrl
      apiVersion = cfg.apiVersion
  apiKey <- decrypt cfg.apiKey
  CAA.chatCompletion chatCompletionUrl apiVersion apiKey req
