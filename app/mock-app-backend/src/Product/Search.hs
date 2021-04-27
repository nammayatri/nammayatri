{-# LANGUAGE OverloadedLabels #-}

module Product.Search
  ( searchCb,
  )
where

import App.Types
import App.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Search
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.At (ix)
import Data.Aeson (encode)
import EulerHS.Prelude
import EulerHS.Types (client)

searchCb :: Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb _ req = withFlowHandlerBecknAPI $ do
  let resp = AckResponse (req ^. #context) (ack ACK) Nothing
  ctx <- updateCaller $ req ^. #context
  logTagDebug "mock_app_backend" $ "search_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  case req ^. #contents of
    Right msg -> do
      case msg ^? #catalog . #_items . ix 0 . #_id of
        Just itemId -> do
          selectReq <- buildSelectReq ctx itemId
          case req ^. #context . #_bpp_uri of
            Just url -> do
              void $
                callClient' (Just HttpSig.signatureAuthManagerKey) "select" (req ^. #context) url $
                  client API.selectAPI selectReq
            Nothing -> logTagError "mock_app_backend" "Bad ac_id"
        Nothing ->
          logTagDebug "mock_app_backend" "search_cb error: no items in the catalog."
    Left err -> logTagDebug "mock_app_backend" $ "search_cb error: " <> show err
  return resp
