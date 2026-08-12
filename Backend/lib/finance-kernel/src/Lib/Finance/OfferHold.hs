{-
  Lib.Finance.OfferHold

  Redis-backed provisional holds for balances at offer time, before any
  ledger entry exists. One sorted set per owner key: each member is an
  (offerId, amount) pair whose score is the offer's expiry timestamp in
  milliseconds, so lapsed holds fall out of every read by score and are
  garbage-collected lazily — no cleanup job and no ledger churn for
  offers that never convert.

  These holds are a reservation gate only, never a record of money
  movement; the ledger remains the single source of truth. Callers own
  the key naming (e.g. "WalletOfferHolds:<ownerId>") and pass the full
  Redis key to every function here.
-}
module Lib.Finance.OfferHold
  ( OfferHold (..),
    addOfferHoldAtKey,
    removeOfferHoldAtKey,
    liveOfferHoldsAtKey,
    getOfferHoldTotalAtKey,
    getOfferHoldAmountAtKey,
  )
where

import qualified Data.Aeson as Ae
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common

-- | One provisional hold: (offerId, amount). Stored as the member of the
--   per-owner sorted set; the score is the hold's expiry in milliseconds.
newtype OfferHold = OfferHold (Text, Double)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

offerHoldMaxScore :: Double
offerHoldMaxScore = 1e15

-- | Add (or refresh) the hold for an offer. Re-offering on the same
--   offerId replaces the previous member, keeping the max amount, so an
--   owner is never double-held for one offer.
addOfferHoldAtKey :: (CacheFlow m r, MonadFlow m) => Text -> Text -> HighPrecMoney -> UTCTime -> m ()
addOfferHoldAtKey key offerId amount validTill = do
  now <- getCurrentTime
  Redis.withWaitOnLockRedisWithExpiry (key <> ":lock") 3 3 $ do
    existingRaw <- Redis.zRangeByScore key (utcToMilliseconds now) offerHoldMaxScore
    let sameOffer = [(r, amt) | r <- existingRaw, Just (OfferHold (oId, amt)) <- [Ae.decodeStrict r], oId == offerId]
    unless (null sameOffer) $ void $ Redis.zRem key (map (TE.decodeUtf8 . fst) sameOffer)
    let holdAmount = maximum (realToFrac amount : map snd sameOffer)
    Redis.zAdd key [(utcToMilliseconds validTill, OfferHold (offerId, holdAmount))]
    Redis.expire key (max 3600 (ceiling (Time.diffUTCTime validTill now) + 60))

-- | Release the hold for an offer (e.g. converted to a real ledger hold,
--   or the offer was withdrawn).
removeOfferHoldAtKey :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m ()
removeOfferHoldAtKey key offerId = do
  Redis.withWaitOnLockRedisWithExpiry (key <> ":lock") 3 3 $ do
    rawItems <- Redis.zRangeByScore key 0 offerHoldMaxScore
    let matching = [r | r <- rawItems, Just (OfferHold (oId, _)) <- [Ae.decodeStrict r], oId == offerId]
    unless (null matching) $ void $ Redis.zRem key (map TE.decodeUtf8 matching)

-- | All unexpired holds at the key, as offerId -> amount.
liveOfferHoldsAtKey :: (CacheFlow m r, MonadFlow m) => Text -> m (Map.Map Text Double)
liveOfferHoldsAtKey key = do
  now <- getCurrentTime
  rawItems <- Redis.zRangeByScore key (utcToMilliseconds now) offerHoldMaxScore
  pure $ Map.fromListWith max [(oId, amt) | r <- rawItems, Just (OfferHold (oId, amt)) <- [Ae.decodeStrict r]]

-- | Sum of all unexpired holds at the key; also garbage-collects expired members.
getOfferHoldTotalAtKey :: (CacheFlow m r, MonadFlow m) => Text -> m HighPrecMoney
getOfferHoldTotalAtKey key = do
  now <- getCurrentTime
  _ <- Redis.zRemRangeByScore key 0 (utcToMilliseconds now)
  holds <- liveOfferHoldsAtKey key
  pure $ sum $ map realToFrac $ Map.elems holds

-- | The unexpired hold amount for one offer, 0 if none.
getOfferHoldAmountAtKey :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m HighPrecMoney
getOfferHoldAmountAtKey key offerId = do
  holds <- liveOfferHoldsAtKey key
  pure $ maybe 0 realToFrac $ Map.lookup offerId holds
