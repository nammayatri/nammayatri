{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( searchCb,
  )
where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Control.Lens.At (ix)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Storage.Queries.Organization

searchCb :: Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb _ req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  ctx <- updateCaller $ req ^. #context
  EL.logDebug @Text "mock_app_backend" $ "search_cb: req: " <> show (toJSON req) <> ", resp: " <> show resp
  case req ^. #contents of
    Right msg -> do
      case msg ^? #catalog . #_items . ix 0 . #_id of
        Just itemId -> do
          selectReq <- buildSelectReq ctx itemId
          case req ^. #context . #_bpp_uri of
            Just url -> do
              bppOrg <- findOrgByCallbackUrl url >>= fromMaybeM500 "UNKNOWN_PROVIDER_URI"
              cbApiKey <- bppOrg ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
              void $
                callClient "select" url $
                  client selectAPI cbApiKey selectReq
            Nothing -> EL.logError @Text "mock_app_backend" "Bad ac_id"
        Nothing ->
          EL.logDebug @Text "mock_app_backend" "search_cb error: no items in the catalog."
    Left err -> EL.logDebug @Text "mock_app_backend" $ "search_cb error: " <> show err
  return resp
