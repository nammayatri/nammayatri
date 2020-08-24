{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.FMD.API.Search
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Mock
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

search :: Organization -> SearchReq -> FlowHandler AckResponse
search org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let context =
        (req ^. #context)
          { _bpp_uri = bppNwAddr
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
      fork "Search" $ do
        baseUrl <- xGatewayUri <$> ask
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
