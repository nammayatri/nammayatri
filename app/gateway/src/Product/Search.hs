{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Core.API.Callback
import qualified Beckn.Types.Core.API.Search as Core
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Error
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail', withClientTracing)
import Beckn.Utils.SignatureAuth (SignaturePayload)
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Product.Validation
import Servant.Client (showBaseUrl)
import qualified Types.API.Gateway.Search as GatewayAPI
import Types.API.Search (OnSearchReq, SearchReq)

search :: SignaturePayload -> Org.Organization -> SearchReq -> FlowHandler AckResponse
search proxySign org req = withFlowHandler $ do
  validateContext "search" (req ^. #context)
  unless (isJust (req ^. #context . #_bap_uri)) $
    throwBecknError400 "INVALID_BAP_URI"
  let gatewaySearchSignAuth = ET.client $ withClientTracing GatewayAPI.searchAPI
      context = req ^. #context
      messageId = context ^. #_transaction_id
  case (Org._callbackUrl org, Org._callbackApiKey org) of
    (Nothing, _) -> throwBecknError500 "CB_URL_NOT_CONFIGURED"
    (_, Nothing) -> throwBecknError500 "CB_API_KEY_NOT_CONFIGURED"
    (Just cbUrl, Just cbApiKey) -> do
      providers <- BP.lookup context
      let bgSession = BA.GwSession cbUrl cbApiKey context
      BA.insert messageId bgSession
      forM_ providers $ \provider -> fork "Provider search" $ do
        providerUrl <- provider ^. #_callbackUrl & fromMaybeM500 CallbackUrlNotSet -- Already checked for existance
        void $ BA.incrSearchReqCount messageId
        -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
        eRes <-
          callAPIWithTrail'
            (Just signatureAuthManagerKey)
            providerUrl
            (gatewaySearchSignAuth (Just proxySign) req)
            "search"
        logDebug "gateway_transaction" $
          messageId
            <> ", search_req: "
            <> T.pack (showBaseUrl providerUrl)
            <> ", req: "
            <> decodeUtf8 (encode req)
            <> ", resp: "
            <> show eRes
        either
          (const $ void $ BA.incrSearchErrCount messageId)
          (const $ return ())
          eRes
      startTimoutHandler bgSession
      if null providers
        then return $ AckResponse context (ack "NACK") (Just $ domainError "No providers")
        else return $ AckResponse context (ack "ACK") Nothing
  where
    startTimoutHandler bgSession = do
      let messageId = bgSession ^. #searchContext . #_transaction_id
      fork (messageId <> " timeout handler") do
        appEnv <- ask
        -- Unit of thread delay is in microseconds
        L.runIO $ threadDelay $ maybe (86400 * 1000000) (* 1000000) $ appEnv ^. #searchTimeout
        checkEnd True bgSession

searchCb :: SignaturePayload -> Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb proxySign provider req@CallbackReq {context} = withFlowHandler $ do
  validateContext "on_search" context
  let gatewayOnSearchSignAuth = ET.client $ withClientTracing GatewayAPI.onSearchAPI
      messageId = req ^. #context . #_transaction_id
  void $ BA.incrOnSearchReqCount messageId
  bgSession <- BA.lookup messageId >>= fromMaybeMWithInfo400 CommonError "Message not found."
  let baseUrl = bgSession ^. #cbUrl
  eRes <-
    callAPIWithTrail'
      (Just signatureAuthManagerKey)
      baseUrl
      (gatewayOnSearchSignAuth (Just proxySign) req)
      "on_search"
  providerUrl <- provider ^. #_callbackUrl & fromMaybeM500 CallbackUrlNotSet -- Already checked for existance
  logDebug "gateway_transaction" $
    messageId
      <> ", search_cb: "
      <> T.pack (showBaseUrl providerUrl)
      <> ", req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show eRes
  checkEnd False bgSession
  AckResponse {} <- checkClientError context eRes
  mkOkResponse (req ^. #context)

checkEnd :: Bool -> BA.GwSession -> Flow ()
checkEnd isTimeout bgSession = do
  let messageId = bgSession ^. #searchContext . #_transaction_id
  (searchReqCount, searchErrCount, onSearchReqCount) <- BA.getRequestStatus messageId
  let sentCount = searchReqCount - searchErrCount
  let receivedCount = onSearchReqCount
  case (isTimeout, sentCount, sentCount == receivedCount) of
    -- isTimout == `true` only when called from timeout handler
    -- case when no requests where sent and none received
    -- possibly error during sending requests/no providers configured
    (True, 0, True) -> sendCb messageId searchReqCount onSearchReqCount
    -- case when timeout interval reached and didn't receive response for
    -- all the requests sent
    (True, _, False) -> sendCb messageId searchReqCount onSearchReqCount
    -- case when received the last callback
    (False, _, True) -> sendCb messageId searchReqCount onSearchReqCount
    -- in any other case do nothing
    _ -> noop messageId
  where
    sendCb messageId searchReqCount onSearchReqCount = do
      let tag = messageId <> "_on_search/end" <> if isTimeout then "_timeout" else ""
      logInfo tag $
        "Sent to " <> show searchReqCount <> ", "
          <> show onSearchReqCount
          <> " responded, "
          <> show (searchReqCount - onSearchReqCount)
          <> " failed/timedout"
      sendSearchEndCb bgSession
      BA.cleanup messageId
    noop messageId = do
      let tag = messageId <> "_on_search/end" <> if isTimeout then "_timeout" else ""
      logInfo tag "Noop"

sendSearchEndCb :: BA.GwSession -> Flow ()
sendSearchEndCb bgSession = do
  let context = bgSession ^. #searchContext & #_action .~ "on_search"
  let messageId = context ^. #_transaction_id
  let onSearchEnd = ET.client $ withClientTracing Core.onSearchEndAPI
  let onSearchEndReq = Core.OnSearchEndReq context
  let baseUrl = bgSession ^. #cbUrl
  eRes <- callAPIWithTrail' (Just signatureAuthManagerKey) baseUrl (onSearchEnd onSearchEndReq) "on_search"
  logDebug "gateway_transaction" $
    messageId
      <> ", on_search/end: "
      <> T.pack (showBaseUrl baseUrl)
      <> ", req: "
      <> decodeUtf8 (encode onSearchEndReq)
      <> ", resp: "
      <> show eRes
