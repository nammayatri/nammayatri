{-# LANGUAGE OverloadedLabels #-}

module Beckn.Utils.Common where

import qualified Beckn.External.FCM.Types as FCM
import Beckn.External.FCM.Utils (createFCMTokenRefreshThread)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Person as Person
import Data.Aeson as A
import qualified Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Data.Time.Calendar (Day (..))
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Network.HTTP.Types (hContentType)
import Servant
import System.Environment

getCurrTime :: Flow LocalTime
getCurrTime = L.runIO $ do
  utc' <- getCurrentTime
  timezone <- getTimeZone utc'
  pure $ utcToLocalTime timezone utc'

defaultLocalTime :: LocalTime
defaultLocalTime = LocalTime (ModifiedJulianDay 58870) (TimeOfDay 1 1 1)

fromMaybeM :: ServerError -> Maybe a -> Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

mkAckResponse :: Text -> Text -> Flow AckResponse
mkAckResponse txnId action = mkAckResponse' txnId action "OK"

mkAckResponse' :: Text -> Text -> Text -> Flow AckResponse
mkAckResponse' txnId action message = do
  currTime <- getCurrTime
  return
    AckResponse
      { _context =
          Context
            { domain = "MOBILITY",
              action = action,
              version = Nothing,
              transaction_id = txnId,
              message_id = Nothing,
              timestamp = currTime,
              dummy = ""
            },
        _message =
          Ack
            { _action = action,
              _message = message
            }
      }

withFlowHandler :: Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift . ExceptT . try . I.runFlow flowRt $ flow

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

authenticate :: Maybe CronAuthKey -> Flow ()
authenticate maybeAuth = do
  keyM <- L.runIO $ lookupEnv "CRON_AUTH_KEY"
  let authHeader = T.stripPrefix "Basic " =<< maybeAuth
      decodedAuthM =
        DT.decodeUtf8
          <$> ( (rightToMaybe . DBB.decode . DT.encodeUtf8)
                  =<< authHeader
              )
  case (decodedAuthM, keyM) of
    (Just auth, Just key) ->
      when (T.pack key /= auth) throw401
    _ -> throw401
  where
    throw401 =
      L.throwException $
        err401 {errBody = "Invalid Auth"}

maskPerson :: Person.Person -> Person.Person
maskPerson person =
  person {Person._deviceToken = FCM.FCMRecipientToken . trimToken . FCM.getFCMRecipientToken <$> person ^. #_deviceToken}
  where
    trimToken token =
      if length token > 6
        then T.take 3 token <> "..." <> T.takeEnd 3 token
        else "..."

-- | Prepare common applications options
prepareAppOptions :: Flow ()
prepareAppOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread

throwJsonError :: ServerError -> Text -> Text -> Flow a
throwJsonError err tag errMsg = do
  L.logError tag errMsg
  L.throwException
    err
      { errBody = A.encode $ getBecknError err errMsg,
        errHeaders = pure jsonHeader
      }
  where
    jsonHeader =
      ( hContentType,
        "application/json;charset=utf-8"
      )

getBecknError :: ServerError -> Text -> BecknError
getBecknError err msg =
  BecknError
    { _errorCode = ErrorCode $ errHTTPCode err,
      _errorMessage = ErrorMsg msg,
      _action = NACK
    }

throwJsonError500, throwJsonError501, throwJsonError400, throwJsonError401 :: Text -> Text -> Flow a
throwJsonError500 = throwJsonError err500
throwJsonError501 = throwJsonError err501
throwJsonError400 = throwJsonError err400
throwJsonError401 = throwJsonError err401

throwJsonErrorH :: ServerError -> Text -> Text -> FlowHandler a
throwJsonErrorH = withFlowHandler ... throwJsonError

throwJsonError500H, throwJsonError501H, throwJsonError400H, throwJsonError401H :: Text -> Text -> FlowHandler a
throwJsonError500H = throwJsonErrorH ... err500
throwJsonError501H = throwJsonErrorH ... err501
throwJsonError400H = throwJsonErrorH ... err400
throwJsonError401H = throwJsonErrorH ... err401

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: LocalTime -> Text
showTimeIst = T.pack . formatTime defaultTimeLocale "%d %b, %I:%M %p" . addLocalTime (60 * 330)
