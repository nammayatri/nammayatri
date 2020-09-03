{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Status where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Status
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

status :: Organization -> StatusReq -> FlowHandler AckResponse
status org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Status" $ do
        statusMessage <- mkStatusMessage
        AckResponse {} <-
          callClient "status" appUrl $
            client
              onStatusAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_status"},
                  contents = Right statusMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkStatusMessage :: Flow StatusResMessage
mkStatusMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ StatusResMessage example
