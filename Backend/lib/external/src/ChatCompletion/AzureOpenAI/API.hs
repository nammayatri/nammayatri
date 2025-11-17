module ChatCompletion.AzureOpenAI.API where

import ChatCompletion.AzureOpenAI.Types as CAT
import EulerHS.Types (EulerClient, client)
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

chatCompletionClient :: Text -> Text -> CAT.ChatCompletionReq -> EulerClient CAT.ChatCompletionResponse
chatCompletionClient = client (Proxy :: Proxy AzureOpenAIAPI)

chatCompletion :: (CoreMetrics m, MonadFlow m) => BaseUrl -> Text -> Text -> CAT.ChatCompletionReq -> m CAT.ChatCompletionResponse
chatCompletion url apiVersion apiKey req = do
  callAPI url (chatCompletionClient apiVersion apiKey req) "chatCompletion" (Proxy :: Proxy AzureOpenAIAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Azure OpenAI chatCompletion" <> " API: " <> show err)
