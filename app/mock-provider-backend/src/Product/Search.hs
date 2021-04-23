{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( search,
  )
where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..), Status (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.FmdError
import qualified Beckn.Types.FMD.API.Search as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Example
import Beckn.Utils.Mock
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified Data.Map as M
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

search :: Organization -> API.SearchReq -> FlowHandler AckResponse
search _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case context ^. #_transaction_id of
    tId
      | tId == noSearchResultId ->
        sendResponse context $ Left noServicesFoundError
      | tId == serviceUnavailableId ->
        sendResponse context $ Left serviceUnavailableError
      | Just fmdErr <- M.lookup tId allFmdErrorFlowIds ->
        sendResponse context $ Left $ fromFmdError fmdErr
    _ ->
      sendResponse context $ Right $ API.OnSearchServices example
  return
    AckResponse
      { _context = context,
        _message = ack ACK,
        _error = Nothing
      }
  where
    sendResponse context contents =
      fork "Search" $ do
        baseUrl <- xGatewayUri <$> ask
        L.runIO $ threadDelay 0.5e6
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "search" (req ^. #context) baseUrl $
            client
              API.onSearchAPI
              CallbackReq
                { context = context {_action = "on_search"},
                  contents = contents
                }
        pass

noServicesFoundError :: Error
noServicesFoundError =
  Error
    { _type = "DOMAIN-ERROR",
      _code = "CORE001",
      _path = Nothing,
      _message = Just "No services found"
    }

serviceUnavailableError :: Error
serviceUnavailableError =
  Error
    { _type = "DOMAIN-ERROR",
      _code = "CORE002",
      _path = Nothing,
      _message = Just "Service unavailable"
    }
