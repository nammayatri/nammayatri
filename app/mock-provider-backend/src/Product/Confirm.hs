{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Confirm
  ( confirm,
  )
where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)

confirm :: Organization -> ConfirmReq -> FlowHandler AckResponse
confirm org req = withFlowHandler $ do
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
      fork "Confirm" $ do
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

mkConfirmResponse :: Flow ConfirmResMessage
mkConfirmResponse = do
  L.runIO $ threadDelay 0.5e6
  return $ ConfirmResMessage example
