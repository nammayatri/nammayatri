{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Cancel
  ( cancel,
  )
where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Cancel
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)

cancel :: Organization -> CancelReq -> FlowHandler AckResponse
cancel org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Cancel" $ do
        cancelMessage <- mkCancelMessage
        AckResponse {} <-
          callClient "cancel" appUrl $
            client
              onCancelAPI
              cbApiKey
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

mkCancelMessage :: Flow CancelResMessage
mkCancelMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ CancelResMessage example example example example
