module Product.Idfy (verifyDL, verifyRC) where

import App.Types
import Beckn.Types.Error
import Beckn.Types.Forkable
import Beckn.Types.GuidLike
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Time
import Common
import EulerHS.Prelude
import Tools.FlowHandling
import Types.API.VerifyDLAsync
import Types.API.VerifyRCAsync
import Types.Common
import Types.IdfyRes
import Types.Webhook

verifyDL :: Maybe ApiKey -> Maybe AccountId -> VerifyDLReq -> FlowHandler IdfyRes
verifyDL apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "rc verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ sendDLVerification $ mkSuccessDL req reqId now
  pure $ IdfyRes {request_id = reqId}

verifyRC :: Maybe ApiKey -> Maybe AccountId -> VerifyRCReq -> FlowHandler IdfyRes
verifyRC apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "rc verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ sendRCVerification $ mkSuccessRC req reqId now
  pure $ IdfyRes {request_id = reqId}

verifyAuth :: Maybe ApiKey -> Maybe AccountId -> Flow ()
verifyAuth apiKey accountId = do
  clientId <- asks (.accountId)
  clientSecret <- asks (.apiKey)
  unless (Just clientId == accountId && Just clientSecret == apiKey) $ throwError (InvalidRequest "Invalid authentication credentials")
