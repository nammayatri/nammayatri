{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Error
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail', withClientTracing)
import Beckn.Utils.SignatureAuth (SignaturePayload)
import Data.Aeson (encode)
import qualified Data.Text as T
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
    throwErrorWithInfo InvalidRequest "Invalid bap URI."
  let gatewaySearchSignAuth = ET.client $ withClientTracing GatewayAPI.searchAPI
      context = req ^. #context
      messageId = context ^. #_transaction_id
  case (Org._callbackUrl org, Org._callbackApiKey org) of
    (Nothing, _) -> throwError OrgCallbackUrlNotSet
    (_, Nothing) -> throwError OrgCallbackApiKeyNotSet
    (Just cbUrl, Just cbApiKey) -> do
      providers <- BP.lookup context
      let bgSession = BA.GwSession cbUrl cbApiKey context
      BA.insert messageId bgSession
      forM_ providers $ \provider -> fork "Provider search" $ do
        providerUrl <- provider ^. #_callbackUrl & fromMaybeM OrgCallbackUrlNotSet -- Already checked for existance
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
      if null providers
        then return $ AckResponse context (ack "NACK") (Just $ domainError "No providers")
        else return $ AckResponse context (ack "ACK") Nothing

searchCb :: SignaturePayload -> Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb proxySign provider req@CallbackReq {context} = withFlowHandler $ do
  validateContext "on_search" context
  let gatewayOnSearchSignAuth = ET.client $ withClientTracing GatewayAPI.onSearchAPI
      messageId = req ^. #context . #_transaction_id
  bgSession <- BA.lookup messageId >>= fromMaybeMWithInfo InvalidRequest "Message not found."
  let baseUrl = bgSession ^. #cbUrl
  eRes <-
    callAPIWithTrail'
      (Just signatureAuthManagerKey)
      baseUrl
      (gatewayOnSearchSignAuth (Just proxySign) req)
      "on_search"
  providerUrl <- provider ^. #_callbackUrl & fromMaybeM OrgCallbackUrlNotSet -- Already checked for existance
  logDebug "gateway_transaction" $
    messageId
      <> ", search_cb: "
      <> T.pack (showBaseUrl providerUrl)
      <> ", req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show eRes
  AckResponse {} <- checkClientError context eRes
  mkOkResponse (req ^. #context)
