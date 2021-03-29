{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import App.Types
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.FMD.API.Cancel
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Data.Aeson (encode)
import EulerHS.Prelude

cancelCb :: Organization -> OnCancelReq -> FlowHandler AckResponse
cancelCb _org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  logDebug "mock_app_backend" $ "cancel_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  case req ^. #context . #_bpp_uri of
    Nothing -> logError "mock-app-backend" "Bad bpp_nw_address"
    Just _ -> logInfo "mock-app-backend" "Cancelled successfully"
  return resp
