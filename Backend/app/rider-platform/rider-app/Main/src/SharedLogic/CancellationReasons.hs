module SharedLogic.CancellationReasons where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig

getCancellationReasons ::
  (HasCacheConfig r, Hedis.HedisFlow m r, CoreMetrics m, HasBapInfo r m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  m CancellationReasons
getCancellationReasons providerUrl bppId =
  Hedis.safeGet (makeCancellationReasonsId bppId) >>= \case
    Just a -> do
      logDebug "GOT FROM REDIS"
      return a
    Nothing -> do
      logDebug "GOT FROM BPP"
      findAndCache providerUrl bppId

makeCancellationReasonsId :: Text -> Text
makeCancellationReasonsId bppId = "CancellationReasons:BppId:" <> bppId

cacheCancellationReasons :: (CacheFlow m r) => Text -> CancellationReasons -> m ()
cacheCancellationReasons bppId reasons = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCancellationReasonsId bppId) reasons expTime

findAndCache ::
  (HasCacheConfig r, Hedis.HedisFlow m r, CoreMetrics m, HasBapInfo r m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  m CancellationReasons
findAndCache providerUrl bppId = do
  reasons <- CallBPP.cancellationReasons providerUrl (CancellationReasonsReq "" "")
  cacheCancellationReasons bppId reasons
  return reasons
