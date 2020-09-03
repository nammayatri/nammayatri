{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Init
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

init :: Organization -> InitReq -> FlowHandler AckResponse
init org req = withFlowHandler $ do
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
      fork "Init" $ do
        msg <- mkInitOrder
        AckResponse {} <-
          callClient "init" appUrl $
            client
              onInitAPI
              cbApiKey
              CallbackReq
                { context = context,
                  contents = Right msg
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkInitOrder :: Flow InitOrder
mkInitOrder = do
  L.runIO $ threadDelay 0.5e6
  return $ InitOrder example
