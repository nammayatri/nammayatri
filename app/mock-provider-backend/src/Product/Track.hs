{-# LANGUAGE OverloadedLabels #-}

module Product.Track where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Track as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

track :: Organization -> API.TrackReq -> FlowHandler AckResponse
track _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let orderId = req ^. (#message . #order_id)
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Track" $ do
        trackMessage <- mkTrackMessage orderId
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "track" (req ^. #context) appUrl $
            client
              API.onTrackAPI
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

mkTrackMessage :: Text -> Flow API.TrackResMessage
mkTrackMessage orderId = do
  L.runIO $ threadDelay 0.5e6
  return $ API.TrackResMessage example orderId
