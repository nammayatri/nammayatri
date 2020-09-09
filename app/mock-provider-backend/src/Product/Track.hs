{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Track where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Track
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

track :: Organization -> TrackReq -> FlowHandler AckResponse
track org req = withFlowHandler $ do
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
      fork "Track" $ do
        trackMessage <- mkTrackMessage
        AckResponse {} <-
          callClient "track" (req ^. #context) appUrl $
            client
              onTrackAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_track"},
                  contents = Right trackMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkTrackMessage :: Flow TrackResMessage
mkTrackMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ TrackResMessage example
