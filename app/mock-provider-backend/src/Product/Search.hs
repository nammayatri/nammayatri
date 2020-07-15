{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import qualified "beckn-gateway" App.Routes as GR
import Beckn.Types.API.Search (OnSearchReq (..), OnSearchServices, SearchReq, SearchRes)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Product.GatewayLookup

search :: SearchReq -> FlowHandlerR r SearchRes
search req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  forkAsync "Search" $ do
    baseUrl <- lookupBaseUrl
    ans <- mkSearchAnswer
    AckResponse {} <-
      callClient "search" baseUrl $
        GR.cliOnSearch
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
