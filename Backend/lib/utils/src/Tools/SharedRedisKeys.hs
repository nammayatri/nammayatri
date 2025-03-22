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
getBatchConfig srId = Hedis.withCrossAppRedis (Hedis.safeGet (batchConfigKey srId))
