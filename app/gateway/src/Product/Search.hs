{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM400, fromMaybeM500, withFlowHandler)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, withClientTracing)
import Data.Aeson (encode)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Servant.Client (BaseUrl, parseBaseUrl)
import System.Environment (lookupEnv)
import Types.API.Search (OnSearchReq, SearchReq, onSearchAPI, searchAPI)

parseOrgUrl :: Text -> Flow BaseUrl
parseOrgUrl =
  fromMaybeM400 "INVALID_TOKEN"
    . parseBaseUrl
    . toString

search :: Org.Organization -> SearchReq -> FlowHandler AckResponse
search org req = withFlowHandler $ do
  let search' = ET.client $ withClientTracing searchAPI
      messageId = req ^. #context . #_request_transaction_id
  appUrl <- Org._callbackUrl org & fromMaybeM400 "INVALID_ORG"
  providers <- BP.lookup $ req ^. #context
  bgId <- L.runIO $ lookupEnv "GATEWAY_ID"
  bgNwAddr <- L.runIO $ lookupEnv "GATEWAY_NW_ADDRESS"
  let context =
        (req ^. #context)
          { _bg_id = fromString <$> bgId,
            _bg_nw_address = fromString <$> bgNwAddr
          }
  resps <- forM providers $ \provider -> do
    providerUrl <- provider ^. #_callbackUrl & fromMaybeM500 "PROVIDER_URL_NOT_FOUND" -- Already checked for existance
    let providerApiKey = fromMaybe "" $ provider ^. #_callbackApiKey
    baseUrl <- parseOrgUrl providerUrl
    eRes <- callAPIWithTrail baseUrl (search' providerApiKey (req & #context .~ context)) "search"
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
      return $ AckResponse context (ack "ACK") Nothing
    else return $ AckResponse context (ack "NACK") (Just $ domainError "No providers")

searchCb :: Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb _org req = withFlowHandler $ do
  let onSearch = ET.client $ withClientTracing onSearchAPI
      messageId = req ^. #context . #_request_transaction_id
  appUrl <- BA.lookup messageId >>= fromMaybeM400 "INVALID_MESSAGE"
  baseUrl <- parseOrgUrl appUrl
  eRes <- callAPIWithTrail baseUrl (onSearch "" req) "on_search"
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
