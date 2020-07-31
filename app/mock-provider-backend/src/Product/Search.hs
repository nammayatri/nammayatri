{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.FMD.API.Search
import Beckn.Utils.Common
import Beckn.Utils.Mock
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Product.GatewayLookup
import System.Environment (lookupEnv)

search :: () -> SearchReq -> FlowHandlerR r AckResponse
search _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppId <- L.runIO $ lookupEnv "MOCK_PROVIDER_ID"
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  let context =
        (req ^. #context)
          { _bpp_id = fromString <$> bppId,
            _bpp_nw_address = fromString <$> bppNwAddr
          }
  case context ^. #_request_transaction_id of
    tId
      | tId == noSearchResultId ->
        pass
    tId
      | tId == searchPickupLocationNotServiceableId ->
        sendResponse context $ errorResponse "FMD001"
    _ ->
      sendResponse context happyServices
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }
  where
    happyServices = (OnSearchServices example, Nothing)

    errorResponse err = (OnSearchServices emptyCatalog, Just $ domainError err)

    sendResponse context (msg, err) =
      forkAsync "Search" $ do
        baseUrl <- lookupBaseUrl
        L.runIO $ threadDelay 0.5e6
        AckResponse {} <-
          callClient "search" baseUrl $
            client
              onSearchAPI
              "test-provider-2-key"
              OnSearchReq
                { context = context {_action = "on_search"},
                  message = msg,
                  error = err
                }
        pass
