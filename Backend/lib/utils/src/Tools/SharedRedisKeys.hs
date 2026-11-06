module Tools.SharedRedisKeys
  ( module Tools.SharedRedisKeys,
    module Reexport,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.SharedRedisKeys as Reexport
import Kernel.Utils.Common

batchConfigKey :: Text -> Text
batchConfigKey srId = "batchConfig:" <> srId

setBatchConfig :: CacheFlow m r => Text -> BatchConfig -> m ()
setBatchConfig id batchConfig = do
  Hedis.withCrossAppRedis $ Hedis.setExp (batchConfigKey id) batchConfig 600

getBatchConfig :: CacheFlow m r => Text -> m (Maybe BatchConfig)
getBatchConfig srId = Hedis.runInMultiCloudRedisMaybeResult $ Hedis.withCrossAppRedis (Hedis.safeGet (batchConfigKey srId))

-- | How a better-driver stand-by search ended without a swap (e.g. NO_MATCH, DRIVER_CLOSE),
-- keyed by the BPP booking id, the one id both apps hold. The BPP writes it when the search
-- ends and clears it when a new search starts; the BAP reads it into the booking status.
betterDriverSearchOutcomeKey :: Text -> Text
betterDriverSearchOutcomeKey bppBookingId = "betterDriverSearchOutcome:" <> bppBookingId

setBetterDriverSearchOutcome :: CacheFlow m r => Text -> Text -> m ()
setBetterDriverSearchOutcome bppBookingId outcome =
  Hedis.withCrossAppRedis $ Hedis.setExp (betterDriverSearchOutcomeKey bppBookingId) outcome 3600

getBetterDriverSearchOutcome :: CacheFlow m r => Text -> m (Maybe Text)
getBetterDriverSearchOutcome bppBookingId =
  Hedis.runInMultiCloudRedisMaybeResult $ Hedis.withCrossAppRedis (Hedis.safeGet (betterDriverSearchOutcomeKey bppBookingId))

clearBetterDriverSearchOutcome :: CacheFlow m r => Text -> m ()
clearBetterDriverSearchOutcome bppBookingId =
  Hedis.withCrossAppRedis $ Hedis.del (betterDriverSearchOutcomeKey bppBookingId)
