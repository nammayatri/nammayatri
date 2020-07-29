{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM400, withFlowHandler)
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
  let search' = ET.client searchAPI
      messageId = req ^. #context . #_request_transaction_id
  appUrl <- Org._callbackUrl org & fromMaybeM400 "INVALID_ORG"
  providerUrls <- BP.lookup $ req ^. #context
  resps <- forM providerUrls $ \providerUrl -> do
    baseUrl <- parseOrgUrl providerUrl
    eRes <- L.callAPI baseUrl $ search' "" req
    L.logDebug @Text "gateway" $
      "request_transaction_id: " <> messageId
        <> ", search: req: "
        <> decodeUtf8 (encode req)
        <> ", resp: "
        <> show eRes
    return $ isRight eRes
  if or resps
    then do
      BA.insert messageId appUrl
      return $ AckResponse (req ^. #context) (ack "ACK") Nothing
    else return $ AckResponse (req ^. #context) (ack "NACK") (Just $ domainError "No providers")

searchCb :: Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb _org req = withFlowHandler $ do
  let onSearch = ET.client onSearchAPI
      messageId = req ^. #context . #_request_transaction_id
  appUrl <- BA.lookup messageId >>= fromMaybeM400 "INVALID_MESSAGE"
  baseUrl <- parseOrgUrl appUrl
  eRes <- L.callAPI baseUrl $ onSearch "" req
  let resp = case eRes of
        Left err -> AckResponse (req ^. #context) (ack "NACK") (Just $ domainError $ show err)
        Right _ -> AckResponse (req ^. #context) (ack "ACK") Nothing
  L.logDebug @Text "gateway" $
    "request_transaction_id: " <> messageId
      <> ", search_cb: req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show resp
  return resp
