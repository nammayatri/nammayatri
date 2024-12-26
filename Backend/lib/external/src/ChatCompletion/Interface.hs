{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Interface where

import ChatCompletion.Interface.AzureOpenAI as CIA
import ChatCompletion.Interface.Gemini as CIG
import ChatCompletion.Interface.Types as CIT
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

chatCompletion :: (EncFlow m r, CoreMetrics m, Log m) => CIT.LLMChatCompletionServiceConfig -> CIT.GeneralChatCompletionReq -> m CIT.GeneralChatCompletionResp
chatCompletion serviceConfig req = case serviceConfig of
  AzureOpenAI cfg -> CIA.azureOpenAIChatCompletion cfg req
  Gemini cfg -> CIG.geminiChatCompletion cfg req
