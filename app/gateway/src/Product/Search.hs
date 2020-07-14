{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.API.Search (OnSearchReq, OnSearchRes, SearchReq, SearchRes, onSearchAPI, searchAPI)
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Ack
import Beckn.Utils.Common (fromMaybeM400, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Servant.Client (BaseUrl, parseBaseUrl)
import qualified Storage.Queries.App as BA

parseOrgUrl :: Text -> Flow BaseUrl
parseOrgUrl =
  fromMaybeM400 "INVALID_TOKEN"
    . parseBaseUrl
    . toString

search :: Text -> SearchReq -> FlowHandler SearchRes
search token req = withFlowHandler $ do
  let search' = ET.client searchAPI
      messageId = req ^. #context . #transaction_id
  appUrl <- BA.lookupToken token >>= fromMaybeM400 "INVALID_TOKEN"
  providerUrls <- BP.lookup $ req ^. #context
  resps <- forM providerUrls $ \providerUrl -> do
    baseUrl <- parseOrgUrl providerUrl
    eRes <- L.callAPI baseUrl $ search' req
    L.logDebug @Text "gateway" $ "search: req: " <> show req <> ", resp: " <> show eRes
    return $ isRight eRes
  if or resps
    then do
      BA.insert messageId appUrl
      return $ AckResponse (req ^. #context) (Ack "Successful" "Ok")
    else return $ AckResponse (req ^. #context) (Ack "Error" "No providers")

searchCb :: Text -> OnSearchReq -> FlowHandler OnSearchRes
searchCb _token req = withFlowHandler $ do
  let onSearch = ET.client onSearchAPI
      messageId = req ^. #context . #transaction_id
  mAppUrl <- BA.lookup messageId >>= fromMaybeM400 "INVALID_MESSAGE"
  baseUrl <- parseOrgUrl mAppUrl
  eRes <- L.callAPI baseUrl $ onSearch req
  let ack = either (Ack "Error" . show) (^. #_message) eRes
      resp = AckResponse (req ^. #context) ack
  L.logDebug @Text "gateway" $ "search_cb: req: " <> show req <> ", resp: " <> show resp
  return resp
