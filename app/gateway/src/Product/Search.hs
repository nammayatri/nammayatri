{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import Beckn.Types.API.Search (OnSearchReq, OnSearchRes, SearchReq, SearchRes, onSearchAPI, searchAPI)
import Beckn.Types.App (FlowHandler)
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Ack
import Beckn.Utils.Common (withFlowHandler)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Servant.Client (BaseUrl (..), Scheme (..))

mkBaseUrl :: String -> Int -> BaseUrl
mkBaseUrl host port =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = host,
      baseUrlPort = port,
      baseUrlPath = "/v1"
    }

-- TODO: auth

search :: SearchReq -> FlowHandler SearchRes
search req = withFlowHandler $ do
  (host, port) <- BP.lookup $ req ^. #context
  let search' = ET.client searchAPI
      baseUrl = mkBaseUrl host port
  eRes <- EL.callAPI baseUrl $ search' req
  let ack = either (Ack "Error" . show) (^. #_message) eRes
      resp = AckResponse (req ^. #context) ack
  EL.logDebug @Text "gateway" $ "search: req: " <> show req <> ", resp: " <> show resp
  return resp

searchCb :: OnSearchReq -> FlowHandler OnSearchRes
searchCb req = withFlowHandler $ do
  (host, port) <- BA.lookup $ req ^. #context
  let onSearch = ET.client onSearchAPI
      baseUrl = mkBaseUrl host port
  eRes <- EL.callAPI baseUrl $ onSearch req
  let ack = either (Ack "Error" . show) (^. #_message) eRes
      resp = AckResponse (req ^. #context) ack
  EL.logDebug @Text "gateway" $ "search_cb: req: " <> show req <> ", resp: " <> show resp
  return resp
