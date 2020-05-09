module Beckn.Utils.Common where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
import Data.Time.LocalTime
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

getCurrentTimeUTC :: L.Flow LocalTime
getCurrentTimeUTC = L.runIO' "getCurrentTimeUTC" getCurrentTimeUTC'

getCurrentTimeUTC' :: IO LocalTime
getCurrentTimeUTC' = (zonedTimeToLocalTime . utcToZonedTime utc) <$> getCurrentTime

mkAckResponse :: Text -> Text -> L.Flow AckResponse
mkAckResponse txnId action = do
  (currTime :: LocalTime) <- getCurrTime
  return
    AckResponse
      { _context =
          Context
            { _domain = "MOBILITY"
            , _action = action
            , _version = Nothing
            , _transaction_id = txnId
            , _message_id = Nothing
            , _timestamp = currTime
            , _dummy = ""
            }
      , _message =
          Ack
            { _action = action
            , _message = ""
            }
      }
