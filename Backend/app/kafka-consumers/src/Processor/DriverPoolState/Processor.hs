-- | DRIVER_POOL_STATE: the state writer. Applies each driver-pool-state event
-- to the sessionizer's own Redis:
--
--   * driver_state:current:<id>  hash of the DriverPoolData state-column fields, merged event by event
--   * driver_state:history:<id>  sorted set of DriverStateSnapshot, one entry per
--                                change to a state column (lastUpdatedAt aside)
--
-- A driver seen for the first time is seeded from the app's pool data, so the
-- first change lands on a complete record. Every Redis call here is checked: a
-- failed write throws, so the message is retried and never committed.
module Processor.DriverPoolState.Processor
  ( processDriverPoolState,
    publishWriterWatermark,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Database.Redis as R
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Processor.DriverPoolState.Types
import Processor.StateColumns (stampVehicleChange, stateColumnKeys)
import qualified "dynamic-offer-driver-app" SharedLogic.DriverPool.DriverPoolDataBuilder as DPDBuilder

-- | History entries kept per driver (the newest).
historyCap :: Integer
historyCap = 100

processDriverPoolState :: DriverPoolStateEvent -> Flow ()
processDriverPoolState ev = do
  stateEnv <- stateRedisEnv
  let ck = encodeUtf8 (currentKey ev.driverId)
      hk = encodeUtf8 (historyKey ev.driverId)
  isSeeded <- runChecked stateEnv "seed marker read" (R.hget ck (encodeUtf8 seededField))
  -- Retry on every event until it works: a driver's first events (login,
  -- onboarding) arrive before they have a vehicle, and the DB builder needs one.
  -- The seed is the app's pool data (LTS Redis), which holds state not yet in
  -- the DB; a driver with no pool data yet is built from the DB, as dispatch
  -- does (that build also writes the app's pool data and publishes its
  -- registrationNo). Seeding before applying the event's changes is safe, as
  -- the app publishes after writing. The seed is the state as of now: live,
  -- that is milliseconds after the event; when catching up on a backlog, the
  -- first history entry carries the event's ts but today's values.
  unless ((A.decodeStrict =<< isSeeded) == Just True) $ do
    seeded <- DPDBuilder.getOrBuildDriverPoolDataBatch False False [Id ev.driverId]
    case map A.toJSON seeded of
      -- The marker is written in the same transaction as the fields.
      (A.Object record : _) -> writeFields stateEnv ck hk (AKM.insert (AK.fromText seededField) (A.Bool True) record)
      _ -> logDebug $ "driver-pool-state: driver " <> ev.driverId <> " not seedable yet (no vehicle); will retry"
  -- One extra read, only on events carrying the vehicle number.
  oldVehicle :: Maybe A.Value <-
    if AKM.member "registrationNo" ev.changes
      then (A.decodeStrict =<<) <$> runChecked stateEnv "vehicle read" (R.hget ck "registrationNo")
      else pure Nothing
  -- One transaction, so registrationNo and registrationNoChangedAt land together.
  writeFields stateEnv ck hk (stampVehicleChange ev.ts (maybe AKM.empty (AKM.singleton "registrationNo") oldVehicle) ev.changes)
  current <- runChecked stateEnv "current state read" (R.hgetall ck)
  let snapshot = snapshotFromFields stateColumnKeys (HM.fromList [(decodeUtf8 field, v) | (field, raw) <- current, Just v <- [A.decodeStrict raw]])
  latest <- runChecked stateEnv "history read" (R.zrange hk (-1) (-1))
  -- An event older than the newest entry is a replay (a restart or rebalance
  -- re-reads the messages handled since the last commit) or arrived late: the
  -- merged state is newer than it, so writing it at the event's ts would
  -- rewrite the past. Its fields are in the current hash all the same.
  unless (maybe False (\(t, s) -> s == snapshot || ev.ts < t) (listToMaybe latest >>= decodeMember)) $
    runTransaction stateEnv "history write" $ do
      void $ R.zadd hk [(fromIntegral ev.ts, jsonBytes (encodeMember ev.ts snapshot))]
      void $ R.expire hk (toInteger stateExpiry)
      -- By rank, so entries sharing a score with the oldest kept one survive.
      R.zremrangebyrank hk 0 (negate historyCap - 1)

-- | Record how far the writer has applied events on the partitions this
-- replica owns (partition -> epoch ms), and the topic's partition count, for
-- the sessionizer to wait on.
publishWriterWatermark :: Int -> M.Map Int Integer -> Flow ()
publishWriterWatermark partitionCount marks = do
  stateEnv <- stateRedisEnv
  let num :: Integer -> ByteString
      num n = encodeUtf8 (show n :: Text)
      fields = (writerPartitionsField, num (toInteger partitionCount)) : [(writerPartitionField p, num ms) | (p, ms) <- M.toList marks]
  void $ runChecked stateEnv "writer watermark write" (R.hmset (encodeUtf8 writerWatermarkKey) fields)

stateRedisEnv :: Flow Hedis.HedisEnv
stateRedisEnv = asks (.driverStateHedisEnv) >>= fromMaybeM (InternalError "driverStateRedisCfg is not set; DRIVER_POOL_STATE needs it")

-- | The object's state-column fields and seed marker (JSON-encoded, as the
-- sessionizer reads them; the other DriverPoolData fields, deviceToken among
-- them, are never stored), and the expiry of both keys, in one transaction:
-- the history lives as long as the current hash, even when no write changes it.
writeFields :: Hedis.HedisEnv -> ByteString -> ByteString -> A.Object -> Flow ()
writeFields stateEnv ck hk record =
  unless (null fields) $
    runTransaction stateEnv "current state write" $ do
      void $ R.hmset ck fields
      void $ R.expire ck (toInteger stateExpiry)
      R.expire hk (toInteger stateExpiry)
  where
    fields = [(encodeUtf8 (AK.toText k), jsonBytes v) | (k, v) <- AKM.toList record, S.member (AK.toText k) storedKeys]

-- | The current-hash fields: what the sessionizer loads ('stateColumnKeys') and the seed marker.
storedKeys :: S.Set Text
storedKeys = S.fromList (seededField : stateColumnKeys)

jsonBytes :: ToJSON a => a -> ByteString
jsonBytes = LBS.toStrict . A.encode

-- | Runs one command on the state Redis; an error reply throws.
runChecked :: Hedis.HedisEnv -> Text -> R.Redis (Either R.Reply a) -> Flow a
runChecked stateEnv what action = do
  reply <- liftIO $ R.runRedis stateEnv.hedisConnection action
  either (\err -> throwError (InternalError ("driver-pool-state: " <> what <> " failed: " <> show err))) pure reply

-- | Runs a MULTI/EXEC on the state Redis; anything but success throws.
runTransaction :: Hedis.HedisEnv -> Text -> R.RedisTx (R.Queued a) -> Flow ()
runTransaction stateEnv what tx = do
  res <- liftIO $ R.runRedis stateEnv.hedisConnection (R.multiExec tx)
  case res of
    R.TxSuccess _ -> pure ()
    R.TxAborted -> throwError (InternalError ("driver-pool-state: " <> what <> " aborted"))
    R.TxError err -> throwError (InternalError ("driver-pool-state: " <> what <> " failed: " <> show err))
