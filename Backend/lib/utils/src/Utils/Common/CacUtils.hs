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
import Data.Aeson
import qualified Data.Bifunctor as DBF
import Data.Text as Text
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified GHC.List as GL
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude as KP
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Tools.Metrics.CoreMetrics.Types (incrementSystemConfigsFailedCounter)
import Kernel.Types.Cac
import Kernel.Types.Error
import Kernel.Utils.Common
import Utils.Common.Cac.ContextConstants as Reexport
import Utils.Common.Cac.KeyNameConstants as Reexport
import Utils.Common.Cac.PrefixConstants as Reexport
import Utils.Common.Cac.UtilsConstants as Reexport

getCacMetricErrorFromCac :: CacPrefix -> Text
getCacMetricErrorFromCac tableName = do
  case tableName of
    DriverPoolConfig -> "driver_pool_config_from_cac_parse_error"
    DriverIntelligentPoolConfig -> "driver_intelligent_pool_config_from_cac_parse_error"
    FarePolicy -> "fare_policy_from_cac_parse_error"
    GoHomeConfig -> "go_home_config_from_cac_parse_error"
    _ -> "empty_cac_parse_error"

getCacMetricErrorFromDB :: CacPrefix -> Text
getCacMetricErrorFromDB tableName = do
  case tableName of
    DriverPoolConfig -> "driver_pool_config_from_db_parse_error"
    DriverIntelligentPoolConfig -> "driver_intelligent_pool_config_from_db_parse_error"
    FarePolicy -> "fare_policy_from_db_parse_error"
    GoHomeConfig -> "go_home_config_from_db_parse_error"
    _ -> "empty_db_parse_error"

pushCacDataToKafka :: EsqDBFlow m r => Maybe CacKey -> [(CacContext, Value)] -> Int -> (String -> [(Text, Value)] -> Int -> IO Value) -> String -> CacPrefix -> m ()
pushCacDataToKafka stickeyKey context toss getVariants tenant key' = do
  when (isJust stickeyKey) do
    let context' = DBF.first show <$> context
    variantIds <- liftIO $ getVariants tenant context' toss
    let key = fromJust stickeyKey
    let cacData = CACData (getKeyValue key) (getKeyValue key) (Text.pack (show context)) (Text.pack (show key')) (Text.pack (show variantIds))
    pushToKafka cacData "cac-data" ""

getConfigListFromCac :: (KvDbFlow m r, FromJSON a, ToJSON a) => [(CacContext, Value)] -> String -> Int -> CacPrefix -> String -> m (Maybe [a])
getConfigListFromCac context' tenant toss prefix id = do
  let context = fmap (DBF.first KP.show) context'
  liftIO $ CM.getConfigListFromCAC context tenant toss (KP.show prefix) id

getConfigFromCac :: (KvDbFlow m r, FromJSON a, ToJSON a) => [(CacContext, Value)] -> String -> Int -> CacPrefix -> m (Maybe a)
getConfigFromCac context' tenant toss prefix = do
  let context = fmap (DBF.first KP.show) context'
  liftIO $ CM.getConfigFromCAC context tenant toss (KP.show prefix)

---------------------------------------------------- Common Cac Application Usage functions ------------------------------------------------------------

getConfigFromCACStrictCommon :: (KvDbFlow m r, FromJSON b, ToJSON b) => Int -> [(CacContext, Value)] -> CacPrefix -> m (Maybe b)
getConfigFromCACStrictCommon toss context cpf = do
  cacConfig <- asks (.cacConfig)
  getConfigFromCac context cacConfig.tenant toss cpf

createThroughConfigHelper :: (KvDbFlow m r, FromJSON b, ToJSON b, HasSchemaName BeamSC.SystemConfigsT) => Int -> [(CacContext, Value)] -> CacPrefix -> m (Maybe b)
createThroughConfigHelper toss context cpf = do
  cacConfig <- asks (.cacConfig)
  config' <- KSQS.findById $ Text.pack cacConfig.tenant
  config <- maybe (throwError $ InternalError "config not found for transporterConfig in db") pure config'
  _ <- initializeCACThroughConfig CM.createClientFromConfig config cacConfig.tenant cacConfig.host (fromIntegral cacConfig.interval)
  getConfigFromCACStrictCommon toss context cpf

getConfigFromCACCommon ::
  ( KvDbFlow m r,
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
  toss <- getToss (getKeyValue <$> stickyId)
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
        pushCacDataToKafka stickyId context toss CM.getVariants' cacConfig.tenant cpf
        pure $ Just res'
    )
    config

getConfigFromCacOrDB ::
  ( KvDbFlow m r,
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
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  maybe
    ( do
        if getTableName cpf `GL.elem` useCACConfig
          then do
            config <-
              getConfigFromCACCommon context stickyKey fromCacTyp cpf `safeCatch` \(_ :: SomeException) -> do
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
  when (isJust val && isExp) do
    L.setOption key (fromJust val)
  pure val
