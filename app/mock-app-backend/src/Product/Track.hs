{-# LANGUAGE OverloadedLabels #-}

module Product.Track where

import App.Types
import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Track
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Data.Aeson (encode)
import EulerHS.Prelude

trackCb :: Organization -> OnTrackReq -> FlowHandler AckResponse
trackCb _org req = withFlowHandlerBecknAPI $ do
  let resp = AckResponse (req ^. #context) (ack ACK) Nothing
  logTagDebug "mock_app_backend" $ "track_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  --  let tracker = req ^. #message .  #tracking
  case req ^. #context . #_bpp_uri of
    Nothing -> logTagError "mock-app-backend" "Bad ac_id"
    Just _ -> logTagDebug "mock-app-backend" "Tracking Started Successfully"
  -- TODO: make call to update api
  --      void $
  --        callClient "update" url $
  --          client updateAPI "test-app-2-key" updateReq
  return resp
