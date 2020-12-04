{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Confirm
  ( confirm,
  )
where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

confirm :: Organization -> ConfirmReq -> FlowHandler AckResponse
confirm org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Confirm" $ do
        resp <- mkConfirmResponse
        AckResponse {} <-
          callClient "confirm" (req ^. #context) appUrl $
            client
              onConfirmAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_confirm"},
                  contents = Right resp
                }
        pass
  pure
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkConfirmResponse :: Flow ConfirmResMessage
mkConfirmResponse = do
  L.runIO $ threadDelay 0.5e6
  return $ ConfirmResMessage example
