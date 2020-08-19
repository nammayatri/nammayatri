{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import Beckn.Types.API.Callback
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.FMD.API.Search
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Mock
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Product.GatewayLookup
import System.Environment (lookupEnv)

search :: Organization -> SearchReq -> FlowHandlerR r AckResponse
search org req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
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
        sendResponse cbApiKey context $ Left $ domainError "FMD001"
    _ ->
      sendResponse cbApiKey context $ Right $ OnSearchServices example
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }
  where
    sendResponse cbKey context contents =
      forkAsync "Search" $ do
        baseUrl <- lookupBaseUrl
        L.runIO $ threadDelay 0.5e6
        AckResponse {} <-
          callClient "search" baseUrl $
            client
              onSearchAPI
              cbKey
              CallbackReq
                { context = context {_action = "on_search"},
                  contents = contents
                }
        pass
