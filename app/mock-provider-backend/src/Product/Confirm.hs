{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Confirm
  ( confirm,
  )
where

import Beckn.Types.API.Callback
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

confirm :: Organization -> ConfirmReq -> FlowHandlerR r AckResponse
confirm org req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_ac_id
      context =
        (req ^. #context)
          { _ac_id = fromString <$> bppNwAddr -- update caller id
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad ac_id"
    Just appUrl ->
      forkAsync "Confirm" $ do
        resp <- mkConfirmResponse
        AckResponse {} <-
          callClient "confirm" appUrl $
            client
              onConfirmAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_confirm"},
                  contents = Right resp
                }
        pass
  pure
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkConfirmResponse :: FlowR r ConfirmResMessage
mkConfirmResponse = do
  L.runIO $ threadDelay 0.5e6
  return $ ConfirmResMessage example
