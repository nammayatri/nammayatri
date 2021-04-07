{-# LANGUAGE OverloadedLabels #-}

module Product.Status where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Status as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Example
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

status :: Organization -> API.StatusReq -> FlowHandler AckResponse
status _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logTagError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Status" $ do
        statusMessage <- mkStatusMessage
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "status" (req ^. #context) appUrl $
            client
              API.onStatusAPI
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

mkStatusMessage :: Flow API.StatusResMessage
mkStatusMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ API.StatusResMessage example
