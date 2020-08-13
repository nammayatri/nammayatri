{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Cancel
  ( cancel,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Callback
import Beckn.Types.FMD.API.Cancel
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

cancel :: () -> CancelReq -> FlowHandlerR r AckResponse
cancel _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  let context =
        (req ^. #context)
          { _ac_id = fromString <$> bppNwAddr
          }
      mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_ac_id
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      forkAsync "Cancel" $ do
        cancelMessage <- mkCancelMessage
        AckResponse {} <-
          callClient "cancel" appUrl $
            client
              onCancelAPI
              "test-provider-2-key"
              CallbackReq
                { context = context {_action = "on_cancel"},
                  contents = Right cancelMessage
                }
        pass
  pure
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkCancelMessage :: FlowR r CancelResMessage
mkCancelMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ CancelResMessage example example example example
