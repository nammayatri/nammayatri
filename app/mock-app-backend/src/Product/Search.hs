{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( searchCb,
    triggerSearch,
  )
where

import App.Types
import App.Utils
import Beckn.Types.API.Search (OnSearchReq, OnSearchRes, SearchRes, searchAPI)
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Ack
import Beckn.Utils.Common (withFlowHandler)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified System.Environment as SE

gatewayLookup :: Flow (String, Int)
gatewayLookup = do
  EL.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "GATEWAY_HOST")
      <*> (fromMaybe 8015 . (>>= readMaybe) <$> SE.lookupEnv "GATEWAY_PORT")

gatewayBaseUrl :: Flow BaseUrl
gatewayBaseUrl = do
  (host, port) <- gatewayLookup
  return
    BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = host,
        baseUrlPort = port,
        baseUrlPath = "/v1"
      }

triggerSearch :: FlowHandler SearchRes
triggerSearch = withFlowHandler $ do
  let search = ET.client searchAPI
  baseUrl <- gatewayBaseUrl
  transactionId <- EL.generateGUID
  req <- EL.runIO $ buildSearchReq transactionId
  eRes <- EL.callAPI baseUrl $ search req
  let ack = either (Ack "Error" . show) (^. #_message) eRes
      resp = AckResponse (req ^. #context) ack
  EL.logDebug @Text "mock_app_backend" $ "search: req: " <> show req <> ", resp: " <> show resp
  return resp

searchCb :: OnSearchReq -> FlowHandler OnSearchRes
searchCb req = withFlowHandler $ do
  let ack = Ack {_action = "on_search", _message = "OK"}
      resp = AckResponse (req ^. #context) ack
  EL.logDebug @Text "mock_app_backend" $ "search_cb: req: " <> show req <> ", resp: " <> show resp
  return resp
