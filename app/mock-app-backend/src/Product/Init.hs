{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import App.Utils
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import qualified Beckn.Types.FMD.API.Confirm as API
import Beckn.Types.FMD.API.Init
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Data.Aeson (encode)
import EulerHS.Prelude
import EulerHS.Types (client)

initCb :: Organization -> OnInitReq -> FlowHandler AckResponse
initCb _org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  ctx <- updateCaller $ req ^. #context
  logDebug "mock_app_backend" $ "init_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  whenRight (req ^. #contents) $ \initResMsg -> do
    confirmReq <- buildConfirmReq ctx (initResMsg ^. #order)
    case req ^. #context . #_bpp_uri of
      Nothing -> logError "mock-app-backend" "Bad ac_id"
      Just url ->
        void $
          callClient' (Just HttpSig.signatureAuthManagerKey) "confirm" (req ^. #context) url $
            client API.confirmAPI confirmReq
  return resp
