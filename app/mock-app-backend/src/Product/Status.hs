{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Status where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Status
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude

statusCb :: () -> OnStatusReq -> FlowHandler AckResponse
statusCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "status_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
    Just _ -> EL.logDebug @Text "mock-app-backend" "Status delivered Successfully"
  return resp
