{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Track where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Track
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude

trackCb :: Organization -> OnTrackReq -> FlowHandler AckResponse
trackCb _org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "track_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  --  let tracker = req ^. #message .  #tracking
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
    Just _ -> EL.logDebug @Text "mock-app-backend" "Tracking Started Successfully"
  -- TODO: make call to update api
  --      void $
  --        callClient "update" url $
  --          client updateAPI "test-app-2-key" updateReq
  return resp
