{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import Beckn.Types.API.Search (OnSearchReq (..), OnSearchServices, SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Product.GatewayLookup

search :: () -> SearchReq -> FlowHandlerR r SearchRes
search _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  forkAsync "Search" $ do
    baseUrl <- lookupBaseUrl
    ans <- mkSearchAnswer
    AckResponse {} <-
      callClient "search" baseUrl $
        client
          onSearchAPI
          "test-provider-1-key"
          OnSearchReq
            { context = req ^. #context,
              message = ans
            }
    pass
  return
    AckResponse
      { _context = req ^. #context,
        _message = Ack {_action = "search", _message = "OK"},
        _error = Nothing
      }

mkSearchAnswer :: FlowR r OnSearchServices
mkSearchAnswer = do
  L.runIO $ threadDelay 0.5e6
  return example
