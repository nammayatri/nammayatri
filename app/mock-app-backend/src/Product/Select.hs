{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Select where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Select
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)

selectCb :: () -> OnSelectReq -> FlowHandler AckResponse
selectCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "select_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  let mBppUrl = parseBaseUrl . toString =<< req ^. #context . #_bpp_nw_address
      quotId = req ^. #message . #quote . #_id
  initReq <- buildInitReq (req ^. #context) quotId
  case mBppUrl of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad bpp_nw_address"
    Just bppUrl ->
      void $
        callClient "init" bppUrl $
          client initAPI "test-app-2-key" initReq
  return resp
