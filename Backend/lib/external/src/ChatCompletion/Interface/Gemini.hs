module ChatCompletion.Interface.Gemini where

import ChatCompletion.Gemini.API as CGA
import ChatCompletion.Gemini.Config as CGC
import ChatCompletion.Gemini.Types as CGT
import ChatCompletion.Interface.Types as CIT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

geminiChatCompletion :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => CGC.GeminiCfg -> CIT.GeneralChatCompletionReq -> m CIT.GeneralChatCompletionResp
geminiChatCompletion cfg req = do
  let chatCompletionUrl = cfg.geminiChatCompletionUrl
  apiKey <- decrypt cfg.geminiApiKey
  let geminiccReqMsgs = fmap toGeminiccReq req.genMessages
      geminiCCReq = CGT.ContentsReq {contents = geminiccReqMsgs}
  geminiCCresp <- CGA.geminiChatCompletion chatCompletionUrl apiKey geminiCCReq
  let candidate = last $ geminiCCresp.candidates
      part = last $ candidate.content.parts
      respRole = candidate.content.role
      respContent = part.text
      genMessage = CIT.GeneralChatCompletionMessage {genContent = respContent, genRole = respRole}
      generalCCResp = CIT.GeneralChatCompletionResp {genMessage = genMessage}
  pure generalCCResp

toGeminiccReq :: CIT.GeneralChatCompletionMessage -> CGT.Content
toGeminiccReq genMsg = CGT.Content {parts = [CGT.Part {text = genMsg.genContent}], role = genMsg.genRole}
