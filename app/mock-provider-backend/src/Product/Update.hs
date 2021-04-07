{-# LANGUAGE OverloadedLabels #-}

module Product.Update where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.FmdError
import qualified Beckn.Types.FMD.API.Update as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Example
import Beckn.Utils.Mock
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified Data.Map as M
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

update :: Organization -> API.UpdateReq -> FlowHandler AckResponse
update _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logTagError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl -> case context ^. #_transaction_id of
      tId
        | tId == locationTooFarId ->
          sendResponse appUrl context $ Left locationTooFarError
        | Just fmdErr <- M.lookup tId allFmdErrorFlowIds ->
          sendResponse appUrl context $ Left $ fromFmdError fmdErr
        | otherwise -> do
          updateMessage <- mkUpdateMessage
          sendResponse appUrl context $ Right updateMessage
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }
  where
    sendResponse appUrl context contents =
      fork "Update" $ do
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "update" (req ^. #context) appUrl $
            client
              API.onUpdateAPI
              CallbackReq
                { context = context {_action = "on_update"},
                  contents = contents
                }
        pass

mkUpdateMessage :: FlowR r API.UpdateResMessage
mkUpdateMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ API.UpdateResMessage example

locationTooFarError :: Error
locationTooFarError =
  Error
    { _type = "DOMAIN-ERROR",
      _code = "CORE003",
      _path = Nothing,
      _message = Just "Location too far from last location"
    }
