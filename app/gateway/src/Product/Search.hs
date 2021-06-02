{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import Beckn.Utils.SignatureAuth (SignaturePayload)
import Data.Aeson (encode)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Product.Validation
import Servant.Client (showBaseUrl)
import qualified Temporary.Utils as TempUtils
import qualified Types.API.Gateway.Search as ExternalAPI
import Types.API.Search (OnSearchReq, SearchReq (..))
import Types.Beckn.API.Callback
import Types.Error
import Utils.Common

search :: SignaturePayload -> Org.Organization -> SearchReq -> FlowHandler AckResponse
search proxySign org req = withFlowHandlerBecknAPI $
  TempUtils.withTransactionIdLogTag req $ do
    validateContext "search" (req ^. #context)
    unless (isJust (req ^. #context . #bap_uri)) $
      throwError $ InvalidRequest "No bap URI in context."
    let gatewaySearchSignAuth = ET.client ExternalAPI.searchAPI
        context = req ^. #context
        messageId = context ^. #transaction_id
    case (Org.callbackUrl org, Org.callbackApiKey org) of
      (Nothing, _) -> throwError (OrgFieldNotPresent "callback_url")
      (_, Nothing) -> throwError (OrgFieldNotPresent "callback_api_key")
      (Just cbUrl, Just cbApiKey) -> do
        providers <- BP.lookup context
        let bgSession = BA.GwSession cbUrl cbApiKey context
        BA.insert messageId bgSession
        when (null providers) $ throwError NoProviders
        forM_ providers $ \provider -> fork "Provider search" $ do
          providerUrl <- provider ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url") -- Already checked for existance
          -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
          eRes <-
            callAPI'
              (Just signatureAuthManagerKey)
              providerUrl
              (gatewaySearchSignAuth (Just proxySign) req)
              "search"
          logTagDebug "gateway_transaction" $
            messageId
              <> ", search_req: "
              <> T.pack (showBaseUrl providerUrl)
              <> ", req: "
              <> decodeUtf8 (encode req)
              <> ", resp: "
              <> show eRes
        return Ack

searchCb :: SignaturePayload -> Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb proxySign provider req@CallbackReq {context} = withFlowHandlerBecknAPI $
  TempUtils.withTransactionIdLogTag req $ do
    validateContext "on_search" context
    let gatewayOnSearchSignAuth = ET.client ExternalAPI.onSearchAPI
        messageId = req ^. #context . #transaction_id
    bgSession <- BA.lookup messageId >>= fromMaybeM (InvalidRequest "Message not found.")
    let baseUrl = bgSession ^. #cbUrl
    eRes <-
      callAPI'
        (Just signatureAuthManagerKey)
        baseUrl
        (gatewayOnSearchSignAuth (Just proxySign) req)
        "on_search"
    providerUrl <- provider ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url") -- Already checked for existance
    logTagDebug "gateway_transaction" $
      messageId
        <> ", search_cb: "
        <> T.pack (showBaseUrl providerUrl)
        <> ", req: "
        <> decodeUtf8 (encode req)
        <> ", resp: "
        <> show eRes
    eRes & fromEitherM (ExternalAPICallError providerUrl)
      >>= checkAckResponseError (ExternalAPIResponseError "on_search")
    return Ack
