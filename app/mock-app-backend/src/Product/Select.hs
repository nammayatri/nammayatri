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

selectCb :: () -> OnSelectReq -> FlowHandler AckResponse
selectCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "select_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  let mquote = req ^. #message . #quote
      quotId = maybe "" (\q -> q ^. #_id) mquote
  initReq <- buildInitReq (req ^. #context) quotId
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock-app-backend" "Bad bpp_nw_address"
    Just url ->
      void $
        callClient "init" url $
          client initAPI "test-app-2-key" initReq
  return resp
