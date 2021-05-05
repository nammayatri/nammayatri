{-# LANGUAGE OverloadedLabels #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.API.Log
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Error
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common as CoreCommon
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import qualified Servant.Client as S (BaseUrl (..), parseBaseUrl)
import Types.Error
import Utils.Metrics as Metrics ()

getClientConfig :: FromJSON a => Organization -> Flow a
getClientConfig org =
  let mconfig = org ^. #_info >>= decodeFromText
   in fromMaybeM (InternalError "Client config decode error.") mconfig

parseBaseUrl :: Text -> Flow S.BaseUrl
parseBaseUrl url =
  L.runIO $
    S.parseBaseUrl $ T.unpack url

fromMaybe400Log :: Text -> Maybe ErrorCode -> Context -> Maybe a -> Flow a
fromMaybe400Log _ _ _ (Just a) = return a
fromMaybe400Log msg errCode ctx Nothing = do
  currTime <- getCurrentTime
  let logCtx =
        Context
          { _domain = FINAL_MILE_DELIVERY,
            _country = Just "IND",
            _city = Nothing,
            _action = "log",
            _core_version = Just "0.8.0",
            _domain_version = Just "0.8.0",
            _bap_uri = Nothing,
            _bpp_uri = Nothing,
            _transaction_id = ctx ^. #_transaction_id,
            _message_id = ctx ^. #_message_id,
            _timestamp = currTime,
            _ttl = Nothing
          }
  gatewayBaseUrl <- xGatewayUri <$> ask
  mGatewayApiKey <- xGatewayApiKey <$> ask
  whenJust mGatewayApiKey $ \gatewayApiKey ->
    fork "Log" $ do
      L.runIO $ threadDelay 0.5e6
      let eClient =
            client
              logAPI
              gatewayApiKey
              LogReq
                { _context = logCtx,
                  _message =
                    Log
                      { _type = "ERROR",
                        _message = msg,
                        _errorCode = maybe "" show errCode,
                        _debug = Nothing,
                        _trace = Nothing,
                        _context = ctx
                      }
                }
      void $ callAPIWithMetrics gatewayBaseUrl eClient "log-fmd-wrapper"

  throwError $
    ErrorCodeWithMessage
      msg
      (fromMaybe CORE001 errCode) -- FIXME
