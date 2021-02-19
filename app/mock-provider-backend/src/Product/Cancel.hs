{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel
  ( cancel,
  )
where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Cancel as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

cancel :: Organization -> API.CancelReq -> FlowHandler AckResponse
cancel _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Cancel" $ do
        cancelMessage <- mkCancelMessage
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "cancel" (req ^. #context) appUrl $
            client
              API.onCancelAPI
              CallbackReq
                { context = context {_action = "on_cancel"},
                  contents = Right cancelMessage
                }
        pass
  pure
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkCancelMessage :: Flow API.CancelResMessage
mkCancelMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ API.CancelResMessage example
