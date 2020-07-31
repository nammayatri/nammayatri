{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)

initCb :: () -> OnInitReq -> FlowHandler AckResponse
initCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "init_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  let mBppUrl = parseBaseUrl . toString =<< req ^. #context . #_bpp_nw_address
      orderId = req ^. #message . #order . #_id
  confirmReq <- buildConfirmReq (req ^. #context) orderId
  case mBppUrl of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad bpp_nw_address"
    Just bppUrl ->
      void $
        callClient "confirm" bppUrl $
          client confirmAPI "test-app-2-key" confirmReq
  return resp
