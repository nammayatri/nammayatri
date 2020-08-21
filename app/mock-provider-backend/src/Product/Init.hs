{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Init where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Init
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)

init :: Organization -> InitReq -> FlowHandler AckResponse
init org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_ac_id
  let context =
        (req ^. #context)
          { _ac_id = bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad ac_id"
    Just appUrl ->
      fork "Init" $ do
        msg <- mkInitResMessage
        AckResponse {} <-
          callClient "init" appUrl $
            client
              onInitAPI
              cbApiKey
              CallbackReq
                { context = context,
                  contents = Right msg
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkInitResMessage :: Flow InitResMessage
mkInitResMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ InitResMessage example
