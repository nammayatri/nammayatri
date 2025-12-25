module ChatCompletion.Gemini.API where

import ChatCompletion.Gemini.Types as CGT
import EulerHS.Types (EulerClient, client)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type GeminiAPI =
  GeminiGenerateContentAPI

type GeminiGenerateContentAPI =
  "v1beta" :> "models" :> "gemini-1.5-flash:generateContent"
    :> MandatoryQueryParam "key" Text
    :> ReqBody '[JSON] CGT.ContentsReq
    :> Post '[JSON] CGT.ContentsResp

geminiChatCompletionClient :: Text -> CGT.ContentsReq -> EulerClient CGT.ContentsResp
geminiChatCompletionClient = client (Proxy :: Proxy GeminiAPI)

geminiChatCompletion :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> CGT.ContentsReq -> m CGT.ContentsResp
geminiChatCompletion url apiKey req = do
  callAPI url (geminiChatCompletionClient apiKey req) "chatCompletion" (Proxy :: Proxy GeminiAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Gemini chatCompletion" <> " API: " <> show err)
