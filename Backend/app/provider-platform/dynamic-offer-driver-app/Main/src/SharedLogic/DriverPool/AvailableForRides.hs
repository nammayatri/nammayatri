{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver-initiated "available for rides" boost.
--
-- A driver switches it on from the app (Domain.Action.UI.AvailableForRides); that writes
-- the @AvailableForRides\#true\#\<expiredAt\>@ tag onto 'person.driverTag', which then rides
-- along into the driver pool the same way every other namma tag does.
--
-- The boost ends on whichever of these comes first:
--
--   * time         -- the expiry stamped into the tag itself. Expired tags are dropped on
--                    every read of 'person.driverTag' and filtered out of the pool's tag JSON,
--                    so nothing has to sweep them.
--   * volume       -- @availableForRidesMaxSearchRequests@ search requests, counted in Redis
--                    by the allocator as it dispatches.
--   * rejections   -- @availableForRidesMaxConsecutiveRejections@ boosted requests rejected in
--                    a row (an accept resets the streak).
--   * cancellation -- a cancellation the fault verdict pins on the driver
--                    (SharedLogic.CancellationOrchestrator).
--
-- A per-driver, per-local-day Redis counter caps how many times the boost can be switched
-- on at all. A boost that runs its full time without a single request being dispatched
-- is handed back: the activation is refunded the next time the boost state is read.
-- Re-activating while a boost is live is allowed but costs another activation and
-- restarts every lifetime.
--
-- Every end except the time-based one is pushed to the driver (FCM and GRPC) with the
-- reason, see 'AvailableForRidesEndReason'. Expiry has no server-side moment to hook --
-- nothing runs when a tag goes stale -- so the app counts that one down by itself.
module SharedLogic.DriverPool.AvailableForRides
  ( availableForRidesTagName,
    availableForRidesTagNameValue,
    mkAvailableForRidesTag,
    hasAvailableForRidesTag,
    holdsLiveTag,
    AvailableForRidesInfo (..),
    AvailableForRidesEndReason (..),
    AvailableForRidesEndedData (..),
    availableForRidesEndedEvent,
    enabledConfig,
    claimActivation,
    startBoost,
    settleExpiredBoost,
    recordRequestSent,
    recordRejection,
    resetRejectionStreak,
    dropTag,
    getAvailableForRidesInfo,
  )
where

import qualified Data.Aeson as A
import Data.OpenApi (ToSchema)
import qualified Data.Time as DT
import qualified Domain.Types.Person as DP
import Domain.Types.TransporterConfig (TransporterConfig)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (Language (ENGLISH), ServiceFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify

availableForRidesTagName :: LYT.TagName
availableForRidesTagName = LYT.TagName "AvailableForRides"

availableForRidesTagValue :: LYT.TagValue
availableForRidesTagValue = LYT.TextValue "true"

availableForRidesTagNameValue :: LYT.TagNameValue
availableForRidesTagNameValue = Yudhishthira.mkTagNameValue availableForRidesTagName availableForRidesTagValue

mkAvailableForRidesTag :: Minutes -> UTCTime -> LYT.TagNameValueExpiry
mkAvailableForRidesTag validity =
  Yudhishthira.mkTagNameValueExpiryInMinutes availableForRidesTagName availableForRidesTagValue (Just validity)

-- | Read the flag off a pool result's tag JSON (the '.driverTags' object built by
-- 'Storage.Queries.Person.GetNearestDrivers.mkResultHelper', which is already
-- expiry-filtered) rather than re-fetching the driver mid-dispatch.
hasAvailableForRidesTag :: A.Value -> Bool
hasAvailableForRidesTag = isJust . Yudhishthira.accessTagKey availableForRidesTagName

findLiveTag :: UTCTime -> Maybe [LYT.TagNameValueExpiry] -> Maybe LYT.TagNameValueExpiry
findLiveTag now = find isLive . fromMaybe []
  where
    isLive tag =
      Yudhishthira.parseTagName tag == Just availableForRidesTagName
        && maybe True (> now) (Yudhishthira.parseTagExpiry tag)

holdsLiveTag :: UTCTime -> Maybe [LYT.TagNameValueExpiry] -> Bool
holdsLiveTag now = isJust . findLiveTag now

data AvailableForRidesInfo = AvailableForRidesInfo
  { isActive :: Bool,
    validTill :: Maybe UTCTime,
    validityMinutes :: Minutes,
    requestsUsed :: Int,
    requestsAllowed :: Int,
    activationsUsedToday :: Int,
    activationsAllowedPerDay :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Why a boost was taken away before its timer ran out.
data AvailableForRidesEndReason
  = -- | @availableForRidesMaxSearchRequests@ requests were dispatched on it.
    REQUEST_LIMIT_REACHED
  | -- | @availableForRidesMaxConsecutiveRejections@ boosted requests rejected in a row.
    CONSECUTIVE_REJECTIONS
  | -- | A ride cancellation the fault verdict pinned on the driver.
    DRIVER_AT_FAULT_CANCELLATION
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Entity payload of the push. It goes out as a plain @DRIVER_NOTIFY@, which app versions
-- that predate the boost already show as an ordinary notification; newer ones recognise
-- it by 'event' and refresh the boost state without waiting for the next profile fetch.
data AvailableForRidesEndedData = AvailableForRidesEndedData
  { event :: Text,
    reason :: AvailableForRidesEndReason
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

availableForRidesEndedEvent :: Text
availableForRidesEndedEvent = "AVAILABLE_FOR_RIDES_ENDED"

-- | English copy used until a city adds its own @merchant_push_notification@ rows
-- (key @AVAILABLE_FOR_RIDES_ENDED_\<reason\>@). The push is what tells the app the boost
-- is over, so a missing row must never mean a missing push.
defaultEndedContent :: AvailableForRidesEndReason -> (Text, Text)
defaultEndedContent reason =
  ( "Boost Search ended",
    case reason of
      REQUEST_LIMIT_REACHED -> "You have received all the ride requests included in this boost."
      CONSECUTIVE_REJECTIONS -> "Your boost was turned off because several ride requests in a row were rejected."
      DRIVER_AT_FAULT_CANCELLATION -> "Your boost was turned off because a ride was cancelled."
  )

type AvailableForRidesEndFlow m r =
  ( ServiceFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  )

notifyEnded :: AvailableForRidesEndFlow m r => DP.Person -> AvailableForRidesEndReason -> m ()
notifyEnded person reason = do
  let merchantOpCityId = person.merchantOperatingCityId
      messageKey = availableForRidesEndedEvent <> "_" <> show reason
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId messageKey Nothing Nothing (Just $ fromMaybe ENGLISH person.language) Nothing
  let (title, body) = maybe (defaultEndedContent reason) (\merchantPN -> (merchantPN.title, merchantPN.body)) mbMerchantPN
  Notify.notifyDriverWithProviders merchantOpCityId Notification.DRIVER_NOTIFY title body person person.deviceToken Nothing $
    AvailableForRidesEndedData {event = availableForRidesEndedEvent, reason}

enabledConfig :: TransporterConfig -> Maybe (Minutes, Int, Int)
enabledConfig transporterConfig = do
  validity <- transporterConfig.availableForRidesTagValidityMinutes
  dailyLimit <- transporterConfig.availableForRidesDailyLimit
  maxRequests <- transporterConfig.availableForRidesMaxSearchRequests
  guard (validity.getMinutes > 0 && dailyLimit > 0 && maxRequests > 0)
  pure (validity, dailyLimit, maxRequests)

data BoostSession = BoostSession
  { activatedOn :: DT.Day,
    validTill :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Keyed by local day so the daily allowance resets at the city's midnight, not UTC's.
activationCountKey :: Id person -> DT.Day -> Text
activationCountKey driverId day = "driver:availableForRides:activations:" <> driverId.getId <> ":" <> show day

requestCountKey :: Id person -> Text
requestCountKey driverId = "driver:availableForRides:requests:" <> driverId.getId

rejectionStreakKey :: Id person -> Text
rejectionStreakKey driverId = "driver:availableForRides:rejectionStreak:" <> driverId.getId

sessionKey :: Id person -> Text
sessionKey driverId = "driver:availableForRides:session:" <> driverId.getId

refundKey :: Id person -> UTCTime -> Text
refundKey driverId validTill = "driver:availableForRides:refunded:" <> driverId.getId <> ":" <> show validTill

activationCountTtl :: Int
activationCountTtl = 172800

settleWindowSeconds :: Int
settleWindowSeconds = 86400

sessionTtl :: Minutes -> Int
sessionTtl validity = validity.getMinutes * 60 + settleWindowSeconds

getActivationsUsedToday :: (Redis.HedisFlow m r) => Id person -> DT.Day -> m Int
getActivationsUsedToday driverId localDay =
  Redis.withCrossAppRedis $ fromMaybe 0 <$> Redis.get @Int (activationCountKey driverId localDay)

claimActivation :: (Redis.HedisFlow m r) => Id person -> DT.Day -> Int -> m (Maybe Int)
claimActivation driverId localDay dailyLimit = Redis.withCrossAppRedis $ do
  let key = activationCountKey driverId localDay
  count <- fromIntegral <$> Redis.incr key
  when (count == 1) $ Redis.expire key activationCountTtl
  if count > dailyLimit
    then Nothing <$ Redis.decr key
    else pure (Just count)

startBoost :: (Redis.HedisFlow m r) => Id person -> DT.Day -> Minutes -> UTCTime -> m ()
startBoost driverId localDay validity validTill = Redis.withCrossAppRedis $ do
  Redis.setExp (sessionKey driverId) (BoostSession {activatedOn = localDay, validTill}) (sessionTtl validity)
  Redis.setExp (requestCountKey driverId) (0 :: Int) (sessionTtl validity)
  Redis.del (rejectionStreakKey driverId)

settleExpiredBoost :: (Redis.HedisFlow m r, MonadTime m) => Id person -> DT.Day -> m ()
settleExpiredBoost driverId localDay = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  mbSession <- Redis.get @BoostSession (sessionKey driverId)
  whenJust mbSession $ \session ->
    when (session.validTill <= now) $ do
      requestsSent <- fromMaybe 0 <$> Redis.get @Int (requestCountKey driverId)
      Redis.del (sessionKey driverId)
      Redis.del (requestCountKey driverId)
      when (requestsSent == 0 && session.activatedOn == localDay) $ do
        firstSettle <- Redis.setNxExpire (refundKey driverId session.validTill) settleWindowSeconds ()
        when firstSettle $ do
          let key = activationCountKey driverId localDay
          used <- fromMaybe 0 <$> Redis.get @Int key
          when (used > 0) $ do
            void $ Redis.decr key
            logInfo $ "AvailableForRides boost expired unused for driver " <> driverId.getId <> ", activation refunded"

-- | Charge one dispatched search request against the driver's budget, and take the tag
-- away once it is spent. Only ever called for drivers that actually carried the tag.
recordRequestSent ::
  AvailableForRidesEndFlow m r =>
  Id person ->
  Minutes ->
  Int ->
  m ()
recordRequestSent driverId validity maxRequests = do
  count <- Redis.withCrossAppRedis $ do
    let key = requestCountKey driverId
    count <- Redis.incr key
    when (count == 1) $ Redis.expire key (sessionTtl validity)
    pure (fromIntegral count :: Int)
  when (count >= maxRequests) $ do
    logInfo $ "AvailableForRides budget exhausted for driver " <> driverId.getId <> ", dropping tag"
    dropTag REQUEST_LIMIT_REACHED driverId

recordRejection ::
  AvailableForRidesEndFlow m r =>
  Id person ->
  Minutes ->
  Int ->
  m ()
recordRejection driverId validity maxRejections = do
  streak <- Redis.withCrossAppRedis $ do
    let key = rejectionStreakKey driverId
    streak <- Redis.incr key
    when (streak == 1) $ Redis.expire key (validity.getMinutes * 60)
    pure (fromIntegral streak :: Int)
  when (streak >= maxRejections) $ do
    logInfo $ "AvailableForRides turned off for driver " <> driverId.getId <> " after " <> show streak <> " consecutive rejections"
    dropTag CONSECUTIVE_REJECTIONS driverId

resetRejectionStreak :: (Redis.HedisFlow m r) => Id person -> m ()
resetRejectionStreak driverId = Redis.withCrossAppRedis $ Redis.del (rejectionStreakKey driverId)

-- | Turn the boost off early: remove the tag and discard the session, so a boost cut short
-- is never refunded, then tell the driver why. A no-op (and no push) when the driver no
-- longer holds the tag.
dropTag ::
  AvailableForRidesEndFlow m r =>
  AvailableForRidesEndReason ->
  Id person ->
  m ()
dropTag reason driverId = do
  let personId = cast driverId
  mbPerson <- QPerson.findById personId
  whenJust mbPerson $ \person ->
    -- findById already drops expired tags, so `retained` is also the cleaned-up list.
    let retained = Yudhishthira.removeTagName person.driverTag availableForRidesTagNameValue
     in when (length retained /= length (fromMaybe [] person.driverTag)) $ do
          QPerson.updateDriverTag (if null retained then Nothing else Just retained) personId
          Redis.withCrossAppRedis $ do
            Redis.del (sessionKey driverId)
            Redis.del (requestCountKey driverId)
            Redis.del (rejectionStreakKey driverId)
          -- Forked so a notification provider that is down or misconfigured can never undo
          -- (or delay) the revocation that has already happened.
          fork "availableForRidesEndedNotification" $ notifyEnded person reason

getAvailableForRidesInfo ::
  (Redis.HedisFlow m r, MonadTime m) =>
  TransporterConfig ->
  Id person ->
  Maybe [LYT.TagNameValueExpiry] ->
  m (Maybe AvailableForRidesInfo)
getAvailableForRidesInfo transporterConfig driverId driverTags =
  forM (enabledConfig transporterConfig) $ \(validity, dailyLimit, maxRequests) -> do
    now <- getCurrentTime
    localDay <- DT.utctDay <$> getLocalCurrentTime transporterConfig.timeDiffFromUtc
    settleExpiredBoost driverId localDay
    activationsUsed <- getActivationsUsedToday driverId localDay
    let mbTag = findLiveTag now driverTags
    requestsUsed <-
      if isJust mbTag
        then Redis.withCrossAppRedis $ fromMaybe 0 <$> Redis.get @Int (requestCountKey driverId)
        else pure 0
    pure
      AvailableForRidesInfo
        { isActive = isJust mbTag,
          validTill = Yudhishthira.parseTagExpiry =<< mbTag,
          validityMinutes = validity,
          requestsUsed = min maxRequests requestsUsed,
          requestsAllowed = maxRequests,
          activationsUsedToday = min dailyLimit activationsUsed,
          activationsAllowedPerDay = dailyLimit
        }
