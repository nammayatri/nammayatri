{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.AzureOpenAI.ToolCallingAPI where

import ChatCompletion.AzureOpenAI.Config as CAC
import ChatCompletion.AzureOpenAI.ToolCallingTypes as CATT
import ChatCompletion.Interface.ToolCalling as CIT
import EulerHS.Types (EulerClient, client)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type ChatCompletionToolAPI =
  "openai"
    :> "deployments"
    :> Capture "deployment" Text
    :> "chat"
    :> "completions"
    :> QueryParam "api-version" Text
    :> Header "api-key" Text
    :> ReqBody '[JSON] CATT.ChatCompletionToolReq
    :> Post '[JSON] CATT.ChatCompletionToolResponse

chatCompletionToolAPI :: Proxy ChatCompletionToolAPI
chatCompletionToolAPI = Proxy

chatCompletionToolClient :: Text -> Maybe Text -> Maybe Text -> CATT.ChatCompletionToolReq -> EulerClient CATT.ChatCompletionToolResponse
chatCompletionToolClient = client chatCompletionToolAPI

chatCompletionWithTools ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CAC.AzureOpenAICfg ->
  Text ->
  [CATT.Message] ->
  [CIT.ToolDefinition] ->
  m CATT.ChatCompletionToolResponse
chatCompletionWithTools cfg deployment messages tools = do
  apiKey <- decrypt cfg.azureApiKey
  let url = cfg.azureOpenAIChatCompletionUrl
      apiVersion = cfg.azureApiVersion
      toolReq =
        CATT.ChatCompletionToolReq
          { messages = messages,
            tools = Just $ map CATT.convertToAzureTool tools,
            toolChoice = Just CATT.ToolChoiceAuto
          }
  callAPI
    url
    (chatCompletionToolClient deployment (Just apiVersion) (Just apiKey) toolReq)
    "chat-completion-with-tools"
    chatCompletionToolAPI
    >>= \case
      Left err -> throwError $ InternalError $ "Error calling Azure OpenAI with tools: " <> show err
      Right resp -> return resp
