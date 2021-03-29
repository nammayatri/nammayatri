{-# LANGUAGE OverloadedLabels #-}

module Product.Select where

import App.Types
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Example
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

select :: Organization -> API.SelectReq -> FlowHandler AckResponse
select _org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = Just bppNwAddr
          }
  case mAppUrl of
    Nothing -> logError "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Select" $ do
        onSelectMessage <- mkQuote
        AckResponse {} <-
          callClient' (Just HttpSig.signatureAuthManagerKey) "select" (req ^. #context) appUrl $
            client
              API.onSelectAPI
              CallbackReq
                { context = context {_action = "on_select"},
                  contents = Right onSelectMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkQuote :: Flow API.SelectOrder
mkQuote = do
  L.runIO $ threadDelay 0.5e6
  return $ API.SelectOrder example
