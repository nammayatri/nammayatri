{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Init
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

init :: () -> InitReq -> FlowHandlerR r AckResponse
init _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
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
      forkAsync "Init" $ do
        msg <- mkInitResMessage
        AckResponse {} <-
          callClient "init" appUrl $
            client
              onInitAPI
              "test-provider-2-key"
              OnInitReq
                { context = context,
                  message = msg,
                  error = Nothing
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkInitResMessage :: FlowR r InitResMessage
mkInitResMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ InitResMessage example
