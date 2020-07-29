{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import App.Types
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Product.GatewayLookup
import System.Environment (lookupEnv)
import "beckn-gateway" Types.API.Search (OnSearchReq (..), SearchReq, onSearchAPI)

search :: () -> SearchReq -> FlowHandlerR r AckResponse
search _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDR"
  let context = (req ^. #context) {_bpp_nw_address = fromString <$> bppNwAddr}
  forkAsync "Search" $ do
    baseUrl <- lookupBaseUrl
    ans <- mkSearchAnswer
    AckResponse {} <-
      callClient "search" baseUrl $
        client
          onSearchAPI
          "test-provider-2-key"
          OnSearchReq
            { context = context,
              message = toJSON ans,
              error = Nothing
            }
    pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkSearchAnswer :: FlowR r OnSearchServices
mkSearchAnswer = do
  L.runIO $ threadDelay 0.5e6
  return example
