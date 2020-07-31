{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Select where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Select
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

select :: () -> SelectReq -> FlowHandlerR r AckResponse
select _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppId <- L.runIO $ lookupEnv "MOCK_PROVIDER_ID"
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDR"
  let context =
        (req ^. #context)
          { _bpp_id = fromString <$> bppId,
            _bpp_nw_address = fromString <$> bppNwAddr
          }
      mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_bap_nw_address
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      forkAsync "Select" $ do
        quote <- mkQuote
        AckResponse {} <-
          callClient "select" appUrl $
            client
              onSelectAPI
              "test-provider-2-key"
              OnSelectReq
                { context = context,
                  message = quote,
                  error = Nothing
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkQuote :: FlowR r OnSelectMessage
mkQuote = do
  L.runIO $ threadDelay 0.5e6
  return $ OnSelectMessage example example
