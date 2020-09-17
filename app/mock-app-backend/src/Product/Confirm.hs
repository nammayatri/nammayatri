{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Confirm
  ( confirmCb,
  )
where

import App.Types
import Beckn.Types.Common
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Control.Lens ((?=))
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import Product.CallsTrack

-- import qualified System.Environment as SE
-- import qualified Beckn.Types.API.Confirm as Confirm

confirmCb :: Organization -> OnConfirmReq -> FlowHandler AckResponse
confirmCb org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "confirm_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  -- ctx <- updateCaller $ req ^. #context
  -- quotId = req ^. #message . #order . #_order_id
  -- confirmReq <- buildConfirmReq ctx quotId
  case req ^. #context . #_bpp_uri of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
    Just _ -> EL.logInfo @Text "mock-app-backend" "Confirm finished successfully"
  whenRight (req ^. #contents) $ \contents ->
    whenJust (contents ^. #order . #_id) $ \orderId ->
      updateCallsTrack $ do
        #orderConfirms . at orderId ?= OrderInfo org
        #lastOrderId ?= orderId
  return resp
