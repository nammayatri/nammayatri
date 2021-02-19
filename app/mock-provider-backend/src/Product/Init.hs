{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Init as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

init :: Organization -> API.InitReq -> FlowHandler AckResponse
init _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Init" $ do
        msg <- mkInitOrder
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "init" (req ^. #context) appUrl $
            client
              API.onInitAPI
              CallbackReq
                { context = context {_action = "on_init"},
                  contents = Right msg
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkInitOrder :: Flow API.InitOrder
mkInitOrder = do
  L.runIO $ threadDelay 0.5e6
  return $ API.InitOrder example
