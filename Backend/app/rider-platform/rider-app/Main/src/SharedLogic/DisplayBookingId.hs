{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DisplayBookingId
  ( generateDisplayBookingId,
    storeLookupMapping,
    findBookingIdByDisplayId,
  )
where

import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow)
import Text.Printf (printf)

-- | Generate a display booking ID in format: DDMMYY-MerchantAbbrev-NNNNNNN
-- Example: 260126-NY-0000001
generateDisplayBookingId ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  ShortId DM.Merchant ->
  Id DRB.Booking ->
  UTCTime ->
  m Text
generateDisplayBookingId merchantShortId bookingId now = do
  let dateStr = formatDate now
      merchantAbbrev = getMerchantAbbreviation merchantShortId
      redisKey = mkCounterKey merchantAbbrev dateStr
  counter <- incrementDailyCounter redisKey
  let counterStr = T.pack $ printf "%07d" counter
      displayId = dateStr <> "-" <> merchantAbbrev <> "-" <> counterStr
  -- Store lookup mapping for quick retrieval (30 days TTL)
  storeLookupMapping displayId bookingId
  pure displayId

-- | Format UTC time as DDMMYY
formatDate :: UTCTime -> Text
formatDate utcTime = T.pack $ formatTime defaultTimeLocale "%d%m%y" utcTime

-- | Get abbreviation from merchant short ID (first letter of each word)
-- e.g., "NAMMA_YATRI" -> "NY", "BOOTH_TAXI" -> "BT"
getMerchantAbbreviation :: ShortId DM.Merchant -> Text
getMerchantAbbreviation (ShortId shortId) =
  T.concat $ map (T.take 1) $ T.splitOn "_" shortId

-- | Redis key for daily counter
mkCounterKey :: Text -> Text -> Text
mkCounterKey merchantAbbrev dateStr =
  "DisplayBookingId:Counter:" <> merchantAbbrev <> ":" <> dateStr

-- | Increment and get the counter value (TTL = 2 days)
incrementDailyCounter :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m Integer
incrementDailyCounter key = do
  counter <- Redis.incr key
  Redis.expire key (2 * 24 * 60 * 60)
  pure counter

-- | Store displayBookingId -> bookingId mapping (TTL = 30 days)
storeLookupMapping :: (MonadFlow m, Redis.HedisFlow m r) => Text -> Id DRB.Booking -> m ()
storeLookupMapping displayId bookingId = do
  let lookupKey = mkLookupKey displayId
  Redis.setExp lookupKey bookingId.getId (30 * 24 * 60 * 60)

-- | Lookup bookingId by displayBookingId
findBookingIdByDisplayId :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m (Maybe (Id DRB.Booking))
findBookingIdByDisplayId displayId = do
  let lookupKey = mkLookupKey displayId
  mbId <- Redis.get lookupKey
  pure $ Id <$> mbId

-- | Redis key for lookup mapping
mkLookupKey :: Text -> Text
mkLookupKey displayId = "DisplayBookingId:Lookup:" <> displayId
