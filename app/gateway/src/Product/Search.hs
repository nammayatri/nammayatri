{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import qualified Beckn.Types.API.Search as Core
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, withClientTracing)
import Data.Aeson (encode)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Servant.Client (BaseUrl, parseBaseUrl)
import Types.API.Search (OnSearchReq, SearchReq, onSearchAPI, searchAPI)

parseOrgUrl :: Text -> Flow BaseUrl
parseOrgUrl =
  fromMaybeM400 "INVALID_TOKEN"
    . parseBaseUrl
    . toString

search :: Org.Organization -> SearchReq -> FlowHandler AckResponse
search org req = withFlowHandler $ do
  let search' = ET.client $ withClientTracing searchAPI
      context = req ^. #context
      messageId = context ^. #_transaction_id
  case (Org._callbackUrl org, Org._callbackApiKey org) of
    (Nothing, _) -> throwJsonError500 "Search.search" "CB_URL_NOT_CONFIGURED"
    (_, Nothing) -> throwJsonError500 "Search.search" "CB_API_KEY_NOT_CONFIGURED"
    (Just cbUrl, Just cbApiKey) -> do
      providers <- BP.lookup context
      let bgSession = BA.GwSession cbUrl cbApiKey context
      BA.insert messageId bgSession
      forM_ providers $ \provider -> fork "Provider search" $ do
        providerUrl <- provider ^. #_callbackUrl & fromMaybeM500 "PROVIDER_URL_NOT_FOUND" -- Already checked for existance
        void $ BA.incrSearchReqCount messageId
        let providerApiKey = fromMaybe "" $ provider ^. #_callbackApiKey
        baseUrl <- parseOrgUrl providerUrl
        eRes <- callAPIWithTrail baseUrl (search' providerApiKey req) "search"
        L.logDebug @Text "gateway" $
          "request_transaction_id: " <> messageId
            <> ", search: req: "
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

searchCb :: Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb org req = withFlowHandler $ do
  let onSearch = ET.client $ withClientTracing onSearchAPI
      messageId = req ^. #context . #_transaction_id
  void $ BA.incrOnSearchReqCount messageId
  bgSession <- BA.lookup messageId >>= fromMaybeM400 "INVALID_MESSAGE"
  baseUrl <- parseOrgUrl (bgSession ^. #cbUrl)
  apiKey <- org ^. #_apiKey & fromMaybeM400 "API_KEY_NOT_CONFIGURED"
  eRes <- callAPIWithTrail baseUrl (onSearch apiKey req) "on_search"
  let resp = case eRes of
        Left err -> AckResponse (req ^. #context) (ack "NACK") (Just $ domainError $ show err)
        Right _ -> AckResponse (req ^. #context) (ack "ACK") Nothing
  L.logDebug @Text "gateway" $
    "request_transaction_id: " <> messageId
      <> ", search_cb: req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show resp
  checkEnd False bgSession
  return resp

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
      L.logInfo @Text tag $
        "Sent to " <> show searchReqCount <> ", "
          <> show onSearchReqCount
          <> " responded, "
          <> show (searchReqCount - onSearchReqCount)
          <> " failed/timedout"
      sendSearchEndCb bgSession
      BA.cleanup messageId
    noop messageId = do
      let tag = messageId <> "_on_search/end" <> if isTimeout then "_timeout" else ""
      L.logInfo @Text tag "Noop"

sendSearchEndCb :: BA.GwSession -> Flow ()
sendSearchEndCb bgSession = do
  let context = bgSession ^. #searchContext
  let messageId = context ^. #_transaction_id
  let onSearchEnd = ET.client $ withClientTracing Core.onSearchEndAPI
  let onSearchEndReq = Core.OnSearchEndReq context
  baseUrl <- parseOrgUrl (bgSession ^. #cbUrl)
  let cbApiKey = bgSession ^. #cbApiKey
  eRes <- callAPIWithTrail baseUrl (onSearchEnd cbApiKey onSearchEndReq) "on_search"
  L.logDebug @Text "gateway" $
    "request_transaction_id: " <> messageId
      <> ", on_search/end: req: "
      <> decodeUtf8 (encode onSearchEndReq)
      <> ", resp: "
      <> show eRes
