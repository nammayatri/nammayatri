module Tools.SharedRedisKeys where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

data BatchConfig = BatchConfig
  { totalBatches :: Int,
    batchTime :: Seconds,
    batchingStartedAt :: UTCTime,
    batchingExpireAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

batchConfigKey :: Text -> Text
batchConfigKey srId = "batchConfig:" <> srId

setBatchConfig :: CacheFlow m r => Text -> BatchConfig -> m ()
setBatchConfig id batchConfig = do
  Hedis.withCrossAppRedis $ Hedis.setExp (batchConfigKey id) batchConfig 600

getBatchConfig :: CacheFlow m r => Text -> m (Maybe BatchConfig)
getBatchConfig srId = Hedis.withCrossAppRedis (Hedis.safeGet (batchConfigKey srId))
