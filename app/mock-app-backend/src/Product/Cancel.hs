{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Cancel where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Cancel
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude

cancelCb :: () -> OnCancelReq -> FlowHandler AckResponse
cancelCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "cancel_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad bpp_nw_address"
    Just _ -> EL.logInfo @Text "mock-app-backend" "Cancelled successfully"
  return resp
