module SharedLogic.CancellationReasons where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig

getCancellationReasons ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasCacheConfig r,
    Hedis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  City ->
  Country ->
  Text ->
  Text ->
  m CancellationReasons
getCancellationReasons providerUrl bppId city country transactionId bapId =
  Hedis.safeGet (makeCancellationReasonsId bppId) >>= \case
    Just a -> return a
    Nothing -> findAndCache providerUrl bppId city country transactionId bapId

makeCancellationReasonsId :: Text -> Text
makeCancellationReasonsId bppId = "CancellationReasons:BppId:" <> bppId

cacheCancellationReasons :: (CacheFlow m r) => Text -> CancellationReasons -> m ()
cacheCancellationReasons bppId reasons = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCancellationReasonsId bppId) reasons expTime

findAndCache ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasCacheConfig r,
    Hedis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  City ->
  Country ->
  Text ->
  Text ->
  m CancellationReasons
findAndCache providerUrl bppId city counrty transactionId bapId = do
  req <- buildCancellationReasonsReq providerUrl bppId city counrty transactionId bapId
  reasons <- CallBPP.cancellationReasons bapId providerUrl req
  cacheCancellationReasons bppId reasons
  return reasons

buildCancellationReasonsReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  BaseUrl ->
  Text ->
  City ->
  Country ->
  Text ->
  Text ->
  m CancellationReasonsReq
buildCancellationReasonsReq bppUrl bppId city country transactionId bapId = do
  bapURI <- asks (.nwAddress)
  messageId <- generateGUID
  context <- buildTaxiContext Context.GET_CANCELLATION_REASONS messageId (Just transactionId) bapId bapURI (Just bppId) (Just bppUrl) city country
  pure $ BecknReq context Empty

buildCancellationReasonsRes ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  BaseUrl ->
  Text ->
  City ->
  Country ->
  Text ->
  Text ->
  CancellationReasons ->
  m CancellationReasonsRes
buildCancellationReasonsRes bppUrl bppId city country transactionId bapId listOfReasons = do
  bapURI <- asks (.nwAddress)
  messageId <- generateGUID
  context <- buildTaxiContext Context.CANCELLATION_REASONS messageId (Just transactionId) bapId bapURI (Just bppId) (Just bppUrl) city country
  let message = makeCancellationReasonsMessage listOfReasons
  pure $ BecknReq context message
