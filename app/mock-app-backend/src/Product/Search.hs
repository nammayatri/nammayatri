{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( searchCb,
    triggerSearch,
  )
where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Utils.Common
import Control.Lens.At (ix)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (BaseUrl (..), Scheme (..), parseBaseUrl)
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
        baseUrlPath = "v1"
      }

triggerSearch :: FlowHandler AckResponse
triggerSearch = withFlowHandler $ do
  baseUrl <- gatewayBaseUrl
  transactionId <- EL.generateGUID
  req <- buildSearchReq transactionId
  eRes <-
    callClient "search" baseUrl $
      client searchAPI "test-app-2-key" req
  EL.logDebug @Text "mock_app_backend" $ "context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = req ^. #context,
        _message = ack "ACK",
        _error = Nothing
      }

searchCb :: () -> OnSearchReq -> FlowHandler AckResponse
searchCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "search_cb: req: " <> show (toJSON req) <> ", resp: " <> show resp
  let mBppUrl = parseBaseUrl . toString =<< req ^. #context . #_bpp_nw_address
      -- FIXME: why is ix 0 not producing a Maybe?
      itemId = req ^. #message . #services . ix 0 . #_catalog . #_items . ix 0 . #_id
  selectReq <- buildSelectReq (req ^. #context) itemId
  case mBppUrl of
    Nothing -> EL.logError @Text "mock_app_backend" "Bad bpp_nw_address"
    Just bppUrl ->
      void $
        callClient "select" bppUrl $
          client selectAPI "test-app-2-key" selectReq
  return resp
