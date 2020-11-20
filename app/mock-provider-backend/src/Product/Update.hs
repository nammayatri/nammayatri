{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Update where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.FmdError
import Beckn.Types.FMD.API.Update
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Mock
import qualified Data.Map as M
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

update :: Organization -> UpdateReq -> FlowHandler AckResponse
update org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl -> case context ^. #_transaction_id of
      tId
        | tId == locationTooFarId ->
          sendResponse appUrl cbApiKey context $ Left locationTooFarError
        | Just fmdErr <- M.lookup tId allFmdErrorFlowIds ->
          sendResponse appUrl cbApiKey context $ Left $ fromFmdError fmdErr
        | otherwise -> do
          updateMessage <- mkUpdateMessage
          sendResponse appUrl cbApiKey context $ Right updateMessage
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }
  where
    sendResponse appUrl cbApiKey context contents =
      fork "Update" $ do
        AckResponse {} <-
          callClient "update" (req ^. #context) appUrl $
            client
              onUpdateAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_update"},
                  contents = contents
                }
        pass

mkUpdateMessage :: FlowR r UpdateResMessage
mkUpdateMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ UpdateResMessage example

locationTooFarError :: Error
locationTooFarError =
  Error
    { _type = "DOMAIN-ERROR",
      _code = "CORE003",
      _path = Nothing,
      _message = Just "Location too far from last location"
    }
