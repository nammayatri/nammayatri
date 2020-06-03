module Beckn.Utils.Common where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Utils.Extra
import Data.Aeson as A
import Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT
import Data.Time
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

getCurrTime :: L.Flow LocalTime
getCurrTime = L.runIO $ do
  utc <- getCurrentTime
  timezone <- getTimeZone utc
  pure $ utcToLocalTime timezone utc

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
mkAckResponse txnId action = do
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
              _message = ""
            }
      }

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift $ ExceptT $ try $ I.runFlow flowRt $ flow

base64Decode :: Maybe Text -> Maybe Text
base64Decode auth =
  DT.reverse <$> DT.drop 1
    <$> DT.reverse
    <$> DT.decodeUtf8
    <$> (rightToMaybe =<< DBB.decode <$> DT.encodeUtf8 <$> DT.drop 6 <$> auth)

fetchMaybeValue :: forall a. Maybe a -> a
fetchMaybeValue c = case c of
  Just d -> d
  Nothing -> undefined -- need to throw error

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode
