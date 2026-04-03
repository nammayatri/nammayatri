{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Gemini.ToolCallingAPI where

import ChatCompletion.Gemini.Config as CGC
import ChatCompletion.Gemini.ToolCallingTypes as CGTT
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
  "v1beta"
    :> "models"
    :> Capture "model" Text
    :> "generateContent"
    :> QueryParam "key" Text
    :> ReqBody '[JSON] CGTT.ContentsToolReq
    :> Post '[JSON] CGTT.ContentsToolResp

chatCompletionToolAPI :: Proxy ChatCompletionToolAPI
chatCompletionToolAPI = Proxy

chatCompletionToolClient :: Text -> Maybe Text -> CGTT.ContentsToolReq -> EulerClient CGTT.ContentsToolResp
chatCompletionToolClient = client chatCompletionToolAPI

chatCompletionWithTools ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CGC.GeminiCfg ->
  Text ->
  [CGTT.Content] ->
  [CIT.ToolDefinition] ->
  m CGTT.ContentsToolResp
chatCompletionWithTools cfg model contents tools = do
  apiKey <- decrypt cfg.apiKey
  let url = cfg.endpoint
      toolReq =
        CGTT.ContentsToolReq
          { contents = contents,
            tools = Just $ map CGTT.convertToGeminiTool tools
          }
  callAPI
    url
    (chatCompletionToolClient model (Just apiKey) toolReq)
    "chat-completion-with-tools"
    chatCompletionToolAPI
    >>= \case
      Left err -> throwError $ InternalError $ "Error calling Gemini with tools: " <> show err
      Right resp -> return resp
