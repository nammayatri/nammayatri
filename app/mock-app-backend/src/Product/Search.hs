{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( searchCb,
    triggerSearch,
  )
where

import qualified "beckn-gateway" App.Routes as GR
import App.Types
import App.Utils
import Beckn.Types.API.Search (OnSearchReq, OnSearchRes, SearchRes)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified System.Environment as SE

gatewayLookup :: FlowR r (String, Int)
gatewayLookup =
  EL.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "GATEWAY_HOST")
      <*> (fromMaybe 8015 . (>>= readMaybe) <$> SE.lookupEnv "GATEWAY_PORT")

gatewayBaseUrl :: FlowR r BaseUrl
gatewayBaseUrl = do
  (host, port) <- gatewayLookup
  return
    BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = host,
        baseUrlPort = port,
        baseUrlPath = ""
      }

triggerSearch :: FlowHandlerR r SearchRes
triggerSearch = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  baseUrl <- gatewayBaseUrl
  transactionId <- EL.generateGUID
  req <- EL.runIO $ buildSearchReq transactionId
  eRes <-
    callClient "search" baseUrl $
      GR.cliSearch "test-app-1-key" req
  EL.logDebug @Text "mock_app_backend" $ "context: " <> show (eRes ^. #_context) <> ", resp: " <> show (eRes ^. #_message)
  return
    AckResponse
      { _context = req ^. #context,
        _message = Ack {_action = "search", _message = "OK"},
        _error = Nothing
      }

searchCb :: OnSearchReq -> FlowHandler OnSearchRes
searchCb req = withFlowHandler $ do
  let ack = Ack {_action = "on_search", _message = "OK"}
      resp = AckResponse (req ^. #context) ack Nothing
  EL.logDebug @Text "mock_app_backend" $ "search_cb: req: " <> show req <> ", resp: " <> show resp
  return resp
