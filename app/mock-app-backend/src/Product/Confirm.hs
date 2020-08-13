{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Confirm
  ( confirmCb,
  )
where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Confirm
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude

-- import qualified System.Environment as SE
-- import qualified Beckn.Types.API.Confirm as Confirm

confirmCb :: () -> OnConfirmReq -> FlowHandler AckResponse
confirmCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "confirm_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  -- quotId = req ^. #message . #order . #_order_id
  -- confirmReq <- buildConfirmReq (req ^. #context) quotId
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
    Just _ -> EL.logError @Text "mock-app-backend" "Confirm finished successfully"
  return resp
