{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Search
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Product.GatewayLookup
import System.Environment (lookupEnv)

search :: () -> SearchReq -> FlowHandlerR r AckResponse
search _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppId <- L.runIO $ lookupEnv "MOCK_PROVIDER_ID"
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDR"
  let context =
        (req ^. #context)
          { _bpp_id = fromString <$> bppId,
            _bpp_nw_address = fromString <$> bppNwAddr
          }
  forkAsync "Search" $ do
    baseUrl <- lookupBaseUrl
    resp <- mkSearchResponse
    AckResponse {} <-
      callClient "search" baseUrl $
        client
          onSearchAPI
          "test-provider-2-key"
          OnSearchReq
            { context = context,
              message = resp,
              error = Nothing
            }
    pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkSearchResponse :: FlowR r OnSearchServices
mkSearchResponse = do
  L.runIO $ threadDelay 0.5e6
  return $ OnSearchServices [example]
