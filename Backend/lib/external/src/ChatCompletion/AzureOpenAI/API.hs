module ChatCompletion.AzureOpenAI.API (chatCompletion) where

import ChatCompletion.AzureOpenAI.Types as CAT
import ChatCompletion.Gemini.API (chatCompletionManagerKey)
import qualified Data.Text as T
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type AzureOpenAIAPI =
  ChatCompletionAPI

type ChatCompletionAPI =
  "openai" :> "deployments" :> "genius-south-india" :> "chat" :> "completions"
    :> MandatoryQueryParam "api-version" Text
    :> MandatoryQueryParam "api-key" Text
    :> ReqBody '[JSON] CAT.ChatCompletionReq
    :> Post '[JSON] CAT.ChatCompletionResponse

chatCompletionClient :: Text -> Text -> CAT.ChatCompletionReq -> ET.EulerClient CAT.ChatCompletionResponse
chatCompletionClient = ET.client (Proxy :: Proxy AzureOpenAIAPI)

chatCompletion :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> CAT.ChatCompletionReq -> m CAT.ChatCompletionResponse
chatCompletion url apiVersion apiKey req = do
  callAPI' (Just $ ET.ManagerSelector chatCompletionManagerKey) url (chatCompletionClient apiVersion apiKey req) "chatCompletion" (Proxy :: Proxy AzureOpenAIAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Azure OpenAI chatCompletion API: " <> stripQueryParams (T.pack $ show err))

-- | Strip query parameters from URLs in error text to prevent credential leakage
stripQueryParams :: Text -> Text
stripQueryParams txt =
  let parts = T.splitOn "?" txt
   in case parts of
        [] -> txt
        [single] -> single
        (firstPart : rest) -> firstPart <> T.concat (map redactQueryPart rest)
  where
    redactQueryPart part =
      let (_, afterParams) = T.break (\c -> c == ' ' || c == '"' || c == '\'' || c == ')' || c == '}') part
       in "?<redacted>" <> afterParams
