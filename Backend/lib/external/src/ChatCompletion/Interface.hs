{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Interface where

import ChatCompletion.AzureOpenAI.Types as CAT
import ChatCompletion.Interface.AzureOpenAI as CIA
import ChatCompletion.Interface.Types as CIT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

chatCompletion :: (EncFlow m r, CoreMetrics m, Log m) => CIT.ChatCompletionServiceConfig -> CAT.ChatCompletionReq -> m CAT.ChatCompletionResponse
chatCompletion serviceConfig req = case serviceConfig of
  AzureOpenAI cfg -> CIA.azureOpenAIChatCompletion cfg req
  _ -> error "chatCompletionConfig not available"
