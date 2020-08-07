{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Status where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Callback
import Beckn.Types.FMD.API.Status
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

status :: () -> StatusReq -> FlowHandlerR r AckResponse
status _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppId <- L.runIO $ lookupEnv "MOCK_PROVIDER_ID"
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  let context =
        (req ^. #context)
          { _bpp_id = fromString <$> bppId,
            _bpp_nw_address = fromString <$> bppNwAddr
          }
      mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_bap_nw_address
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      forkAsync "Status" $ do
        statusMessage <- mkStatusMessage
        AckResponse {} <-
          callClient "status" appUrl $
            client
              onStatusAPI
              "test-app-2-key"
              CallbackReq
                { context = context {_action = "on_status"},
                  contents = Right statusMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkStatusMessage :: FlowR r StatusResMessage
mkStatusMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ StatusResMessage example
