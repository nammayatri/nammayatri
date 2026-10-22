-- | DRIVER_POOL_STATE: the state writer. Applies each driver-pool-state event
-- to the sessionizer's own Redis (never the app or LTS Redis):
--
--   * driver_state:current:<id>  hash of DriverPoolData fields, merged event by event
--   * driver_state:history:<id>  sorted set of DriverStateSnapshot, one entry per
--                                change to the fields the sessionizer uses or to
--                                a state column (lastUpdatedAt aside)
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
import qualified Data.HashMap.Strict as HM
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Processor.DriverPoolState.Types
import Processor.StateColumns (stateColumnKeys)
import qualified "dynamic-offer-driver-app" SharedLogic.DriverPool.DriverPoolDataBuilder as DPDBuilder

-- | History kept per driver until the sessionizer or DRIVER_PERFORMANCE publishes its mark.
historyCapWithoutSessionizer :: Int
historyCapWithoutSessionizer = 100

processDriverPoolState :: DriverPoolStateEvent -> Flow ()
processDriverPoolState ev = do
  let ck = currentKey ev.driverId
      hk = historyKey ev.driverId
  isSeeded :: Maybe Bool <- inStateRedis $ Hedis.hGet ck seededField
  -- Retry on every event until it works: a driver's first events (login,
  -- onboarding) arrive before they have a vehicle, and the DB builder needs one.
  -- The DB already holds each change (the app publishes after writing), so
  -- seeding before applying the event's changes is safe. The seed is the DB as
  -- of now: live, that is milliseconds after the event; when catching up on a
  -- backlog, the first history entry carries the event's ts but today's values.
  -- Seed outside inStateRedis: the DB queries cache through the app's Redis.
  unless (isSeeded == Just True) $ do
    seeded <- DPDBuilder.buildDriverPoolDataFromDB False False [Id ev.driverId]
    case map A.toJSON seeded of
      (A.Object record : _) -> inStateRedis $ do
        writeFields ck record
        Hedis.hSetExp ck seededField True stateExpiry
      _ -> logDebug $ "driver-pool-state: driver " <> ev.driverId <> " not seedable yet (no vehicle); will retry"
  inStateRedis $ do
    writeFields ck ev.changes
    current :: [(Text, A.Value)] <- Hedis.hGetAll ck
    let snapshot = snapshotFromFields stateColumnKeys (HM.fromList current)
    latest <- Hedis.zRange hk (-1) (-1)
    unless ((snd <$> (listToMaybe latest >>= decodeMember)) == Just snapshot) $ do
      Hedis.zAdd hk [(fromIntegral ev.ts, encodeMember ev.ts snapshot)]
      Hedis.expire hk stateExpiry
    trimHistory hk

-- | Record how far the writer has applied events, for the sessionizer to wait on.
publishWriterWatermark :: Integer -> Flow ()
publishWriterWatermark ms = inStateRedis $ Hedis.set writerWatermarkKey ms

-- | Keep the newest entry at or before T plus every entry after T, where T is
-- the earlier of the sessionizer's watermark and DRIVER_PERFORMANCE's state
-- horizon (whichever exist). Before either exists, keep the newest
-- 'historyCapWithoutSessionizer' entries.
trimHistory :: Text -> Flow ()
trimHistory hk = do
  mbSessionizerT :: Maybe Integer <- Hedis.get sessionizerWatermarkKey
  mbHorizon :: Maybe Integer <- Hedis.get stateHorizonKey
  case catMaybes [mbSessionizerT, mbHorizon] of
    marks@(_ : _) -> do
      atOrBefore <- Hedis.zRangeByScore hk negInf (fromIntegral (minimum marks))
      whenJust (lastMaybe atOrBefore >>= decodeMember) $ \(keepFrom, _) ->
        void $ Hedis.zRemRangeByScore hk negInf (fromIntegral keepFrom - 0.5)
    [] -> do
      older <- Hedis.zRangeWithScores hk 0 (negate (fromIntegral historyCapWithoutSessionizer) - 1)
      whenJust (lastMaybe older) $ \(_, score) ->
        void $ Hedis.zRemRangeByScore hk negInf score
  where
    negInf = -1 / 0
    lastMaybe xs = if null xs then Nothing else Just (last xs)

-- | Marker in the current-state hash: set once the driver has been seeded from the DB.
seededField :: Text
seededField = "_seededFromDb"

inStateRedis :: Flow a -> Flow a
inStateRedis action = do
  stateEnv <- asks (.driverStateHedisEnv) >>= fromMaybeM (InternalError "driverStateRedisCfg is not set; DRIVER_POOL_STATE needs it")
  local (Hedis.withSecondaryRedisEnv stateEnv) action

writeFields :: Text -> A.Object -> Flow ()
writeFields ck record =
  forM_ (AKM.toList record) $ \(k, v) -> Hedis.hSetExp ck (AK.toText k) v stateExpiry
