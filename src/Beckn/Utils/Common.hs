module Beckn.Utils.Common where

import           Beckn.Types.Common
import qualified Data.ByteString.Lazy as BSL
import           Data.Time
import           Data.Time.Calendar   (Day (..))
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified EulerHS.Language     as L
import           EulerHS.Prelude
import           Servant

class GuidLike a where
  generateGUID :: L.Flow a
instance GuidLike Text where
  generateGUID = L.generateGUID

getCurrTime :: L.Flow LocalTime
getCurrTime = L.runIO $ do
  utc <- getCurrentTime
  timezone <- getTimeZone utc
  pure $ utcToLocalTime timezone utc

defaultLocalTime :: LocalTime
defaultLocalTime = LocalTime (ModifiedJulianDay 58870) (TimeOfDay 1 1 1)

fromMaybeM :: ServerError -> Maybe a -> L.Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a)  = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> L.Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

sendAck :: L.Flow Ack
sendAck =
  pure $ Ack
    { _action = "ACK"
    , _message = "OK"
    }
