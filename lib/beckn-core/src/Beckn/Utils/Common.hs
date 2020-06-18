{-# LANGUAGE OverloadedLabels #-}

module Beckn.Utils.Common where

import qualified Beckn.External.FCM.Types as FCM
import Beckn.External.FCM.Utils (createFCMTokenRefreshThread)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Extra
import Data.Aeson as A
import Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import System.Environment

defaultLocalTime :: LocalTime
defaultLocalTime = LocalTime (ModifiedJulianDay 58870) (TimeOfDay 1 1 1)

fromMaybeM :: ServerError -> Maybe a -> L.Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> L.Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

mkAckResponse :: Text -> Text -> L.Flow AckResponse
mkAckResponse txnId action = mkAckResponse' txnId action "OK"

mkAckResponse' :: Text -> Text -> Text -> L.Flow AckResponse
mkAckResponse' txnId action message = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
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

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift $ ExceptT $ try $ I.runFlow flowRt $ flow

base64Decode :: Maybe Text -> Maybe Text
base64Decode auth =
  T.reverse <$> T.drop 1
    <$> T.reverse
    <$> DT.decodeUtf8
    <$> (rightToMaybe =<< DBB.decode <$> DT.encodeUtf8 <$> T.drop 6 <$> auth)

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

authenticate :: Maybe CronAuthKey -> L.Flow ()
authenticate maybeAuth = do
  keyM <- L.runIO $ lookupEnv "CRON_AUTH_KEY"
  let authHeader = (T.stripPrefix "Basic ") =<< maybeAuth
      decodedAuthM =
        DT.decodeUtf8
          <$> ( (rightToMaybe . DBB.decode . DT.encodeUtf8)
                  =<< authHeader
              )
  case (decodedAuthM, keyM) of
    (Just auth, Just key) -> do
      when ((T.pack key) /= auth) throw401
      return ()
    _ -> throw401
  where
    throw401 =
      L.throwException $
        err401 {errBody = "Invalid Auth"}

maskPerson :: Person.Person -> Person.Person
maskPerson person =
  person {Person._deviceToken = (FCM.FCMRecipientToken . trimToken . FCM.getFCMRecipientToken) <$> (person ^. #_deviceToken)}
  where
    trimToken token =
      if length token > 6
        then T.take 3 token <> "..." <> T.takeEnd 3 token
        else "..."

-- | Prepare common applications options
prepareAppOptions :: L.Flow ()
prepareAppOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread
