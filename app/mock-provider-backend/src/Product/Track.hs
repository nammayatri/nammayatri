{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Track where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Track as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant ((:<|>) (..))

track :: Organization -> API.TrackReq -> FlowHandler AckResponse
track org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let orderId = req ^. (#message . #order_id)
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Track" $ do
        trackMessage <- mkTrackMessage orderId
        AckResponse {} <-
          callClient "track" (req ^. #context) appUrl $
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
  where
    _ :<|> onTrackAPI = client API.onTrackAPI

mkTrackMessage :: Text -> Flow API.TrackResMessage
mkTrackMessage orderId = do
  L.runIO $ threadDelay 0.5e6
  return $ API.TrackResMessage example orderId
