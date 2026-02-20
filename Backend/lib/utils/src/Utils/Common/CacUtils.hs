{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.CacUtils
  ( module Utils.Common.CacUtils,
    module Reexport,
  )
where

import qualified Client.Main as CM
import qualified Control.Concurrent as CC
import Data.Aeson
import qualified Data.Bifunctor as DBF
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text as Text
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified GHC.List as GL
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude as KP
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Tools.Metrics.CoreMetrics.Types (incrementSystemConfigsFailedCounter)
import Kernel.Types.Cac
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified System.Environment as Se
import Utils.Common.Cac.ContextConstants as Reexport
import Utils.Common.Cac.KeyNameConstants as Reexport
import Utils.Common.Cac.PrefixConstants as Reexport
import Utils.Common.Cac.UtilsConstants as Reexport

getTextValue :: Value -> Text
getTextValue = \case
  String x -> x
  obj -> Text.pack $ show obj

pushCacDataToKafka :: (MonadFlow m, EsqDBFlow m r) => Maybe CacKey -> [(CacContext, Value)] -> Int -> (String -> [(Text, Value)] -> Int -> IO Value) -> String -> CacPrefix -> m ()
pushCacDataToKafka stickeyKey context toss getVariants tenant key' = do
  enableKafka <- liftIO $ Se.lookupEnv "CAC_PUSH_TO_KAFKA"
  when (enableKafka == Just "True") $ for_ stickeyKey $ \key -> do
    let context' = DBF.first show <$> context
    let context'' = DBF.second getTextValue <$> context
    variantIds <- liftIO $ getVariants tenant context' toss
    let cacData = CACData (getKeyValue key) (getKeyName key) (Text.pack (show context'')) (Text.pack (show key')) (getTextValue variantIds)
    fork "push cac data to kafka" $ pushToKafka cacData "cac-data" ""

getConfigListFromCac :: (CacheFlow m r, EsqDBFlow m r, FromJSON a, ToJSON a) => [(CacContext, Value)] -> String -> Int -> CacPrefix -> String -> m (Maybe [a])
getConfigListFromCac context' tenant toss prefix id = do
  let context = fmap (DBF.first KP.show) context'
  config <- liftIO $ CM.getConfigListFromCAC context tenant toss (KP.show prefix) id
  fromJSONHelper config (getTableName prefix)

getConfigFromCac :: (CacheFlow m r, EsqDBFlow m r, FromJSON a, ToJSON a) => [(CacContext, Value)] -> String -> Int -> CacPrefix -> m (Maybe a)
getConfigFromCac context' tenant toss prefix = do
  let context = fmap (DBF.first KP.show) context'
  config <- liftIO $ CM.getConfigFromCAC context tenant toss (KP.show prefix)
  fromJSONHelper config (getTableName prefix)

getConfigFromCacAsString :: (CacheFlow m r, EsqDBFlow m r, FromJSON a, ToJSON a) => Data.Aeson.Object -> String -> Int -> m (Maybe a)
getConfigFromCacAsString context' tenant toss = do
  let context = BSL.unpack (encode context')
  config <- liftIO $ CM.evalExperimentAsString tenant context toss
  pure $ decode (BSL.pack config)

---------------------------------------------------- Common Cac Application Usage functions ------------------------------------------------------------

getConfigFromCACStrictCommon :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, FromJSON b, ToJSON b) => Int -> [(CacContext, Value)] -> CacPrefix -> m (Maybe b)
getConfigFromCACStrictCommon toss context cpf = do
  logDebug $ "Trying to find for last time for table " <> getTableName cpf <> " in cac."
  cacConfig <- asks (.cacConfig)
  getConfigFromCac context cacConfig.tenant toss cpf

runPolling :: String -> IO ()
runPolling tenant = do
  _ <- CM.startCACPolling [tenant]
  _ <- CM.runSuperPositionPolling [tenant]
  pure ()

createThroughConfigHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, FromJSON b, ToJSON b, HasSchemaName BeamSC.SystemConfigsT) => Int -> [(CacContext, Value)] -> CacPrefix -> m (Maybe b)
createThroughConfigHelper toss context cpf = do
  cacConfig <- asks (.cacConfig)
  config' <- KSQS.findById $ Text.pack cacConfig.tenant
  logError $ "Config not found for " <> getTableName cpf <> " in cac. Creating client through config"
  config <- maybe (throwError $ InternalError ("config not found for" <> getTableName cpf <> " in db")) pure config'
  _ <- initializeCACThroughConfig CM.createClientFromConfig config cacConfig.tenant cacConfig.host (fromIntegral cacConfig.interval)
  _ <- liftIO . CC.forkIO $ runPolling cacConfig.tenant
  getConfigFromCACStrictCommon toss context cpf

getConfigFromCACCommon ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    FromJSON a,
    FromJSON b,
    ToJSON b,
    MonadFlow m,
    HasSchemaName BeamSC.SystemConfigsT
  ) =>
  [(CacContext, Value)] ->
  Maybe CacKey ->
  (b -> m (Maybe a)) ->
  CacPrefix ->
  m (Maybe a)
getConfigFromCACCommon context stickyId fromCactyp cpf = do
  cacConfig <- asks (.cacConfig)
  isExp <- liftIO $ CM.isExperimentsRunning cacConfig.tenant
  toss <- bool (pure 1) (getToss (getKeyValue <$> stickyId)) isExp
  res :: (Maybe b) <- getConfigFromCac context cacConfig.tenant toss cpf
  config' <-
    maybe
      ( do
          incrementSystemConfigsFailedCounter $ getCacMetricErrorFromCac cpf
          createThroughConfigHelper toss context cpf
      )
      (pure . Just)
      res
  config <- maybe (pure Nothing) fromCactyp config'
  maybe
    ( do
        incrementSystemConfigsFailedCounter $ getCacMetricErrorFromDB cpf
        pure Nothing
    )
    ( \res' -> do
        when isExp $ pushCacDataToKafka stickyId context toss CM.getVariants' cacConfig.tenant cpf
        pure $ Just res'
    )
    config

getConfigFromCacOrDB ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    FromJSON a,
    FromJSON b,
    ToJSON b,
    HasSchemaName BeamSC.SystemConfigsT,
    Show a
  ) =>
  Maybe a ->
  [(CacContext, Value)] ->
  Maybe CacKey ->
  (b -> m (Maybe a)) ->
  CacPrefix ->
  m (Maybe a)
getConfigFromCacOrDB cachedConfig context stickyKey fromCacTyp cpf = do
  systemConfigs' <- L.getOption KBT.Tables
  systemConfigs <- maybe (KSQS.findById "kv_configs" >>= pure . decodeFromText' @Tables) (pure . Just) systemConfigs'
  let useCACConfig = foldMap (.useCAC) systemConfigs
  maybe
    ( do
        if getTableName cpf `GL.elem` useCACConfig
          then do
            config <-
              getConfigFromCACCommon context stickyKey fromCacTyp cpf `safeCatch` \(err :: SomeException) -> do
                logError $ "CAC failed us: " <> show err
                incrementSystemConfigsFailedCounter $ getCacMetricErrorFromDB cpf
                pure Nothing
            when (isJust config) do
              logDebug $ "Config found in CAC for " <> getTableName cpf <> " with context " <> show context <> " is: " <> show config
            pure config
          else pure Nothing
    )
    (pure . Just)
    cachedConfig

setConfigInMemoryCommon :: (CacheFlow m r, T.OptionEntity k v, MonadFlow m) => k -> Bool -> Maybe v -> m (Maybe v)
setConfigInMemoryCommon key isExp val = do
  when isExp $ for_ val $ L.setOption key
  pure val
