{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Select where

import App.Types
import App.Utils
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Data.Aeson (encode)
import EulerHS.Prelude
import EulerHS.Types (client)

selectCb :: Organization -> API.OnSelectReq -> FlowHandler AckResponse
selectCb _org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  ctx <- updateCaller $ req ^. #context
  logDebug "mock_app_backend" $
    "select_cb: req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show resp
  case req ^. #contents of
    Right msg -> do
      quote <- (msg ^. #order . #_quotation) & fromMaybeM400 "INVALID_QUOTE"
      let quoteId = quote ^. #_id
      initReq <- buildInitReq ctx quoteId
      case req ^. #context . #_bpp_uri of
        Nothing -> logError "mock-app-backend" "Bad ac_id"
        Just url ->
          void $
            callClient' (Just HttpSig.signatureAuthManagerKey) "init" (req ^. #context) url $
              client API.initAPI initReq
    Left err -> logDebug "mock_app_backend" $ "select_cb error: " <> show err
  return resp
