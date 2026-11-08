-- | DRIVER_POOL_STATE: the state writer. Applies each driver-pool-state event
-- to the sessionizer's own Redis (never the app or LTS Redis):
--
--   * driver_state:current:<id>  hash of DriverPoolData fields, merged event by event
--   * driver_state:history:<id>  sorted set of DriverStateSnapshot, one entry per
--                                change to a state column (lastUpdatedAt aside)
--
-- A driver seen for the first time is seeded from the DB, so the first change
-- lands on a complete record.
module Processor.DriverPoolState.Processor
  ( processDriverPoolState,
    publishWriterWatermark,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
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
historyCap :: Int
historyCap = 100

processDriverPoolState :: DriverPoolStateEvent -> Flow ()
processDriverPoolState ev = do
  let ck = currentKey ev.driverId
      hk = historyKey ev.driverId
  isSeeded :: Maybe Bool <- inStateRedis $ Hedis.hGet ck seededField
  -- Retry on every event until it works: a driver's first events (login,
  -- onboarding) arrive before they have a vehicle, and the DB builder needs one.
  -- The seed is the app's pool data (LTS Redis), which holds state not yet in
  -- the DB; a driver with no pool data yet is built from the DB, as dispatch
  -- does. Seeding before applying the event's changes is safe, as the app
  -- publishes after writing. The seed is the state as of now: live, that is
  -- milliseconds after the event; when catching up on a backlog, the first
  -- history entry carries the event's ts but today's values.
  -- Seed outside inStateRedis: the reads go through the app's Redis.
  unless (isSeeded == Just True) $ do
    seeded <- DPDBuilder.getOrBuildDriverPoolDataBatch False False [Id ev.driverId]
    case map A.toJSON seeded of
      (A.Object record : _) -> inStateRedis $ do
        writeFields ck record
        Hedis.hSetExp ck seededField True stateExpiry
      _ -> logDebug $ "driver-pool-state: driver " <> ev.driverId <> " not seedable yet (no vehicle); will retry"
  inStateRedis $ do
    -- One extra read, only on events carrying the vehicle number.
    oldVehicle :: Maybe A.Value <-
      if AKM.member "registrationNo" ev.changes then Hedis.hGet ck "registrationNo" else pure Nothing
    writeFields ck (stampVehicleChange ev.ts (maybe AKM.empty (AKM.singleton "registrationNo") oldVehicle) ev.changes)
    current :: [(Text, A.Value)] <- Hedis.hGetAll ck
    let snapshot = snapshotFromFields stateColumnKeys (HM.fromList current)
    latest <- Hedis.zRange hk (-1) (-1)
    unless ((snd <$> (listToMaybe latest >>= decodeMember)) == Just snapshot) $ do
      Hedis.zAdd hk [(fromIntegral ev.ts, encodeMember ev.ts snapshot)]
      Hedis.expire hk stateExpiry
      trimHistory hk

-- | Record how far the writer has applied events on the partitions this
-- replica owns (partition -> epoch ms), and the topic's partition count, for
-- the sessionizer to wait on.
publishWriterWatermark :: Int -> M.Map Int Integer -> Flow ()
publishWriterWatermark partitionCount marks = do
  stateEnv <- asks (.driverStateHedisEnv) >>= fromMaybeM (InternalError "driverStateRedisCfg is not set; DRIVER_POOL_STATE needs it")
  let num :: Integer -> ByteString
      num n = encodeUtf8 (show n :: Text)
      fields = (writerPartitionsField, num (toInteger partitionCount)) : [(writerPartitionField p, num ms) | (p, ms) <- M.toList marks]
  reply <- liftIO $ R.runRedis stateEnv.hedisConnection $ R.hmset (encodeUtf8 writerWatermarkKey) fields
  case reply of
    Left err -> throwError (InternalError ("writer watermark write failed: " <> show err))
    Right _ -> pure ()

-- | Run when an entry is added: drop the oldest entries past 'historyCap'.
trimHistory :: Text -> Flow ()
trimHistory hk = do
  older <- Hedis.zRangeWithScores hk 0 (negate (fromIntegral historyCap) - 1)
  whenJust (lastMaybe older) $ \(_, score) ->
    void $ Hedis.zRemRangeByScore hk negInf score
  where
    negInf = -1 / 0
    lastMaybe xs = if null xs then Nothing else Just (last xs)

inStateRedis :: Flow a -> Flow a
inStateRedis action = do
  stateEnv <- asks (.driverStateHedisEnv) >>= fromMaybeM (InternalError "driverStateRedisCfg is not set; DRIVER_POOL_STATE needs it")
  local (Hedis.withSecondaryRedisEnv stateEnv) action

writeFields :: Text -> A.Object -> Flow ()
writeFields ck record =
  forM_ (AKM.toList record) $ \(k, v) -> Hedis.hSetExp ck (AK.toText k) v stateExpiry
