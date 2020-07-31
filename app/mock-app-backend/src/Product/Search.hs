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
import Beckn.Utils.Common
import Control.Lens.At (ix)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)

searchCb :: () -> OnSearchReq -> FlowHandler AckResponse
searchCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "search_cb: req: " <> show (toJSON req) <> ", resp: " <> show resp
  -- FIXME: why is ix 0 not producing a Maybe?
  let itemId = req ^. #message . #catalog . #_items . ix 0 . #_id
  selectReq <- buildSelectReq (req ^. #context) itemId
  case bppUrl $ req ^. #context of
    Nothing -> EL.logError @Text "mock_app_backend" "Bad bpp_nw_address"
    Just url ->
      void $
        callClient "select" url $
          client selectAPI "test-app-2-key" selectReq
  return resp
