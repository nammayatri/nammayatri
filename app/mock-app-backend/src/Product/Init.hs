{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import App.Utils
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)

initCb :: Organization -> OnInitReq -> FlowHandler AckResponse
initCb org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  ctx <- updateCaller $ req ^. #context
  EL.logDebug @Text "mock_app_backend" $ "init_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  whenRight (req ^. #contents) $ \initResMsg -> do
    confirmReq <- buildConfirmReq ctx (initResMsg ^. #order)
    cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "API_KEY_NOT_CONFIGURED"
    case req ^. #context . #_bpp_uri of
      Nothing -> EL.logError @Text "mock-app-backend" "Bad ac_id"
      Just url ->
        void $
          callClient "confirm" (req ^. #context) url $
            client confirmAPI cbApiKey confirmReq
  return resp
