module ChatCompletion.Gemini.API
  ( geminiChatCompletion,
    chatCompletionManagerKey,
  )
where

import ChatCompletion.Gemini.Types as CGT
import qualified Data.Text as T
import qualified EulerHS.Types as ET
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

geminiChatCompletionClient :: Text -> CGT.ContentsReq -> ET.EulerClient CGT.ContentsResp
geminiChatCompletionClient = ET.client (Proxy :: Proxy GeminiAPI)

chatCompletionManagerKey :: Text
chatCompletionManagerKey = "chat-completion-http-manager"

geminiChatCompletion :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> CGT.ContentsReq -> m CGT.ContentsResp
geminiChatCompletion url apiKey req = do
  callAPI' (Just $ ET.ManagerSelector chatCompletionManagerKey) url (geminiChatCompletionClient apiKey req) "chatCompletion" (Proxy :: Proxy GeminiAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Gemini chatCompletion API: " <> stripQueryParams (T.pack $ show err))

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
