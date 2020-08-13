{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.FMD.API.Callback
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
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  let context =
        (req ^. #context)
          { _ac_id = fromString <$> bppNwAddr
          }
  case context ^. #_transaction_id of
    tId
      | tId == noSearchResultId ->
        pass
    tId
      | tId == searchPickupLocationNotServiceableId ->
        sendResponse context $ Left $ domainError "FMD001"
    _ ->
      sendResponse context $ Right $ OnSearchServices example
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }
  where
    sendResponse context contents =
      forkAsync "Search" $ do
        baseUrl <- lookupBaseUrl
        L.runIO $ threadDelay 0.5e6
        AckResponse {} <-
          callClient "search" baseUrl $
            client
              onSearchAPI
              "test-provider-2-key"
              CallbackReq
                { context = context {_action = "on_search"},
                  contents = contents
                }
        pass
