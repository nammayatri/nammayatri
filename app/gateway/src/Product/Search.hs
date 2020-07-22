{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM400, withFlowHandler)
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
      messageId = req ^. #context . #_transaction_id
  appUrl <- Org._callbackUrl org & fromMaybeM400 "INVALID_ORG"
  providerUrls <- BP.lookup $ req ^. #context
  resps <- forM providerUrls $ \providerUrl -> do
    baseUrl <- parseOrgUrl providerUrl
    eRes <- L.callAPI baseUrl $ search' "" req
    L.logDebug @Text "gateway" $ "search: req: " <> show (toJSON req) <> ", resp: " <> show eRes
    return $ isRight eRes
  if or resps
    then do
      BA.insert messageId appUrl
      return $ AckResponse (req ^. #context) (Ack "Successful" "Ok") Nothing
    else return $ AckResponse (req ^. #context) (Ack "Error" "No providers") Nothing

searchCb :: Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb _org req = withFlowHandler $ do
  let onSearch = ET.client onSearchAPI
      messageId = req ^. #context . #_transaction_id
  appUrl <- BA.lookup messageId >>= fromMaybeM400 "INVALID_MESSAGE"
  baseUrl <- parseOrgUrl appUrl
  eRes <- L.callAPI baseUrl $ onSearch "" req
  let ack = either (Ack "Error" . show) (^. #_message) eRes
      resp = AckResponse (req ^. #context) ack Nothing
  L.logDebug @Text "gateway" $ "search_cb: req: " <> show (toJSON req) <> ", resp: " <> show resp
  return resp
