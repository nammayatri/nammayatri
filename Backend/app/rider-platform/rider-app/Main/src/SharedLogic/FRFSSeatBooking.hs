module SharedLogic.FRFSSeatBooking where

import API.Types.UI.FRFSTicketService (SeatStatus (..), SeatWithStatus (..))
import qualified Data.Aeson as Aeson
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Redis as RawRedis
import qualified Domain.Types.Seat as Seat
import qualified Domain.Types.SeatLayout as SeatLayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logInfo, logWarning)
import qualified SharedLogic.FRFSSeatBooking.Lua as Lua
import qualified Storage.Queries.Seat as QSeat

data SeatHoldMeta = SeatHoldMeta
  { tripId :: Text,
    seatIds :: [Text],
    fromIdx :: Int,
    toIdx :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

-- For same cluster slot access
tripTag :: Text -> Text
tripTag tripId = "{" <> tripId <> "}"

-- | For general availability checks, we use the standard key.
-- For a 'hold' operation, we wrap this in a hashtag (see holdSeat).
seatKey :: Text -> Id Seat.Seat -> Text
seatKey tripId seatId =
  "trip:" <> tripTag tripId <> ":seat:" <> seatId.getId

metaKey :: Text -> Text -> Text
metaKey tripId holdId =
  "trip:" <> tripTag tripId <> ":hold-meta:" <> holdId

timerKey :: Text -> Text -> Text
timerKey tripId holdId =
  "trip:" <> tripTag tripId <> ":hold-timer:" <> holdId

holdLockKey :: Text -> Text -> Text
holdLockKey tripId holdId =
  "hold-lock:" <> tripId <> ":" <> holdId

data ActiveSeatHold = ActiveSeatHold
  { tripId :: Text,
    holdId :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

-- | Attempt to hold multiple seats atomically
holdSeats ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Text -> -- Trip ID
  [Id Seat.Seat] -> -- Seat IDs
  Int -> -- From Stop Index
  Int -> -- To Stop Index
  Text -> -- Hold ID
  Int -> -- TTL in seconds
  m Bool
holdSeats tripId seatIds fromIdx toIdx holdId ttl = do
  let seatIdStrs = map (.getId) seatIds
  logInfo $ "SeatBooking:holdSeats attempting holdId=" <> holdId <> " tripId=" <> tripId <> " seats=" <> show seatIdStrs <> " range=[" <> show fromIdx <> "," <> show toIdx <> ") ttl=" <> show ttl
  let seatKeys = map (seatKey tripId) seatIds
      mKey = metaKey tripId holdId
      tKey = timerKey tripId holdId
      meta =
        SeatHoldMeta
          { tripId = tripId,
            seatIds = seatIdStrs,
            fromIdx = fromIdx,
            toIdx = toIdx
          }
      args =
        [ TE.encodeUtf8 holdId,
          toBS ttl,
          BS.toStrict (Aeson.encode meta),
          toBS fromIdx,
          toBS toIdx,
          toBS (length seatIds)
        ]
  pSeatKeys <- traverse Redis.buildKey seatKeys
  pMetaKey <- Redis.buildKey mKey
  pTimerKey <- Redis.buildKey tKey
  let allKeys = pSeatKeys ++ [pMetaKey, pTimerKey]
  res :: Integer <-
    Redis.runHedis
      ( RawRedis.eval Lua.holdSeatScript allKeys args ::
          RawRedis.Redis (Either RawRedis.Reply Integer)
      )
  case res of
    1 -> do
      logInfo $ "SeatBooking:holdSeats SUCCESS holdId=" <> holdId <> " tripId=" <> tripId
      void $ Redis.sAdd "active-seat-holds" [ActiveSeatHold tripId holdId]
      pure True
    _ -> do
      logWarning $ "SeatBooking:holdSeats CONFLICT holdId=" <> holdId <> " tripId=" <> tripId <> " seats=" <> show seatIdStrs
      pure False

releaseHold ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Text ->
  Text ->
  m ()
releaseHold tripId holdId = do
  logInfo $ "SeatBooking:releaseHold attempting holdId=" <> holdId <> " tripId=" <> tripId
  Redis.withLockRedis (holdLockKey tripId holdId) 10 $ do
    let mKey = metaKey tripId holdId
        tKey = timerKey tripId holdId
    mbMeta :: Maybe SeatHoldMeta <- Redis.get mKey
    case mbMeta of
      Nothing -> do
        logInfo $ "SeatBooking:releaseHold no-op (meta not found) holdId=" <> holdId <> " tripId=" <> tripId
        pure ()
      Just meta -> do
        logInfo $ "SeatBooking:releaseHold clearing bits holdId=" <> holdId <> " tripId=" <> tripId <> " seats=" <> show meta.seatIds <> " range=[" <> show meta.fromIdx <> "," <> show meta.toIdx <> ")"
        let seatIds = meta.seatIds
            fromIdx = meta.fromIdx
            toIdx = meta.toIdx
            seatKeys =
              map (seatKey tripId . Id) seatIds
            args =
              [ toBS fromIdx,
                toBS toIdx,
                toBS (length seatIds)
              ]
        pSeatKeys <- traverse Redis.buildKey seatKeys
        void $
          Redis.runHedis
            ( RawRedis.eval Lua.clearMultiScript pSeatKeys args ::
                RawRedis.Redis (Either RawRedis.Reply Integer)
            )
        Redis.del mKey
        Redis.del tKey
        void $ Redis.srem "active-seat-holds" [ActiveSeatHold tripId holdId]
        logInfo $ "SeatBooking:releaseHold completed holdId=" <> holdId <> " tripId=" <> tripId

confirmBooking ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Text ->
  Text ->
  m ()
confirmBooking tripId holdId = do
  logInfo $ "SeatBooking:confirmBooking attempting holdId=" <> holdId <> " tripId=" <> tripId
  Redis.withLockRedis (holdLockKey tripId holdId) 10 $ do
    let mKey = metaKey tripId holdId
        tKey = timerKey tripId holdId
    mbMeta :: Maybe SeatHoldMeta <- Redis.get mKey
    case mbMeta of
      Nothing -> do
        logWarning $ "SeatBooking:confirmBooking no-op (meta not found, hold may have expired) holdId=" <> holdId <> " tripId=" <> tripId
        pure ()
      Just _ -> do
        void $ Redis.srem "active-seat-holds" [ActiveSeatHold tripId holdId]
        Redis.del mKey
        Redis.del tKey
        logInfo $ "SeatBooking:confirmBooking completed (bits retained) holdId=" <> holdId <> " tripId=" <> tripId

bookingHoldsKey :: Text -> Text
bookingHoldsKey bookingId = "booking-holds:" <> bookingId

trackHoldForBooking :: (MonadFlow m, Redis.HedisFlow m r) => Text -> Text -> m ()
trackHoldForBooking bookingId holdId = do
  logInfo $ "SeatBooking:trackHoldForBooking bookingId=" <> bookingId <> " holdId=" <> holdId
  let k = bookingHoldsKey bookingId
  void $ Redis.sAddExp k [holdId] 300

releaseAbandonedHolds :: (MonadFlow m, Redis.HedisFlow m r) => Text -> Text -> Text -> m ()
releaseAbandonedHolds tripId bookingId successfulHoldId = do
  let k = bookingHoldsKey bookingId
  allHolds <- Redis.sMembers k
  let abandonedHolds = filter (/= successfulHoldId) allHolds
  logInfo $ "SeatBooking:releaseAbandonedHolds bookingId=" <> bookingId <> " tripId=" <> tripId <> " successfulHoldId=" <> successfulHoldId <> " abandonedCount=" <> show (length abandonedHolds)
  forM_ abandonedHolds $ \h -> releaseHold tripId h
  Redis.del k

releaseConfirmedSeats ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Text ->
  [Id Seat.Seat] ->
  Int ->
  Int ->
  m ()
releaseConfirmedSeats tripId seatIds fromIdx toIdx = do
  logInfo $ "SeatBooking:releaseConfirmedSeats tripId=" <> tripId <> " seats=" <> show (map (.getId) seatIds) <> " range=[" <> show fromIdx <> "," <> show toIdx <> ")"
  let seatKeys = map (seatKey tripId) seatIds
      args =
        [ toBS fromIdx,
          toBS toIdx,
          toBS (length seatIds)
        ]
  pSeatKeys <- traverse Redis.buildKey seatKeys
  void $
    Redis.runHedis
      ( RawRedis.eval Lua.clearMultiScript pSeatKeys args ::
          RawRedis.Redis (Either RawRedis.Reply Integer)
      )
  logInfo $ "SeatBooking:releaseConfirmedSeats completed tripId=" <> tripId

-- | Standard functions (Availability check doesn't need hold hashtags)
getTripAvailability :: (MonadFlow m, Redis.HedisFlow m r) => Text -> Int -> Int -> [Seat.Seat] -> m [SeatWithStatus]
getTripAvailability tripId fromIdx toIdx seats = do
  let keys = map (\s -> seatKey tripId s.id) seats
  bitmaps <- mapM Redis.tryGetFromCluster keys
  return $ zipWith (checkSeatStatus fromIdx toIdx) seats bitmaps

checkSeatStatus :: Int -> Int -> Seat.Seat -> Maybe BS.ByteString -> SeatWithStatus
checkSeatStatus fromIdx toIdx seat mbBitmap =
  let status = case mbBitmap of
        Nothing -> AVAILABLE
        Just bs -> if isRangeFree bs fromIdx toIdx then AVAILABLE else BOOKED
   in SeatWithStatus {seat = seat, status = status}

isRangeFree :: BS.ByteString -> Int -> Int -> Bool
isRangeFree bs from to = all (not . isBitSet bs) [from .. to - 1]

isBitSet :: BS.ByteString -> Int -> Bool
isBitSet bs i =
  let byteIdx = i `div` 8
      bitIdx = 7 - (i `mod` 8)
   in byteIdx < BS.length bs && testBit (BS.index bs byteIdx) bitIdx

getAvailableSeatCount :: (MonadFlow m, Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id SeatLayout.SeatLayout -> Text -> Int -> Int -> m Int
getAvailableSeatCount seatLayoutId tripId fromIdx toIdx = do
  seats <- QSeat.findAllByLayoutId seatLayoutId
  seatsWithStatus <- getTripAvailability tripId fromIdx toIdx seats
  return $ length $ filter (\s -> s.status == AVAILABLE) seatsWithStatus

toBS :: Show a => a -> BS.ByteString
toBS = TE.encodeUtf8 . T.pack . show
