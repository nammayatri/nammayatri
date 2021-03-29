{-# LANGUAGE OverloadedLabels #-}

module Product.Confirm
  ( confirm,
  )
where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Confirm as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Example
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

confirm :: Organization -> API.ConfirmReq -> FlowHandler AckResponse
confirm _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Confirm" $ do
        resp <- mkConfirmResponse
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "confirm" (req ^. #context) appUrl $
            client
              API.onConfirmAPI
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

mkConfirmResponse :: Flow API.ConfirmResMessage
mkConfirmResponse = do
  L.runIO $ threadDelay 0.5e6
  return $ API.ConfirmResMessage example
