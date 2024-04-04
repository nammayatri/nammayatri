{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
  ( findById,
    clearCache,
    update,
    update',
    clearCacheById,
  )
where

import qualified Client.Main as CM
import qualified Data.Aeson as DA
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text
import qualified Domain.Types.Cac as DTC
import Domain.Types.Common
import Domain.Types.FarePolicy
import EulerHS.Language as L (getOption)
import qualified EulerHS.Language as L
import qualified GHC.List as GL
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Tools.Metrics.CoreMetrics.Types (incrementSystemConfigsFailedCounter)
import Kernel.Types.Cac
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.FarePolicy as Queries
import qualified System.Environment as SE
import System.Random

findFarePolicyFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findFarePolicyFromDB id = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
    Nothing -> do
      flip whenJust cacheFarePolicy /=<< Queries.findById id

findFarePolicyFromCACStrict :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> String -> Int -> m (Maybe FarePolicy)
findFarePolicyFromCACStrict id cond toss = do
  tenant <- liftIO $ SE.lookupEnv "TENANT"
  contextValue <- liftIO $ CM.evalExperimentAsString (fromMaybe "atlas_driver_offer_bpp_v2" tenant) cond toss
  config <- jsonToFarePolicy contextValue $ Text.unpack $ getId id
  maybe
    ( do
        incrementSystemConfigsFailedCounter "cac_fare_policy_parse_error"
        findFarePolicyFromDB id
    )
    (pure . Just)
    config

findFarePolicyFromCACHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> String -> Int -> m (Maybe FarePolicy)
findFarePolicyFromCACHelper id cond toss = do
  mbHost <- liftIO $ SE.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ SE.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig (fromMaybe (error "Config not found for DriverIntelligentPoolConfig in db") config) tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  findFarePolicyFromCACStrict id cond toss

findFarePolicyFromCAC :: KvDbFlow m r => Id FarePolicy -> Int -> Maybe Text -> Maybe Text -> m (Maybe FarePolicy)
findFarePolicyFromCAC id toss txnId idName = do
  fp <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "farePolicyId", DA.String (getId id))]
  tenant <- liftIO $ SE.lookupEnv "TENANT"
  contextValue <- liftIO $ CM.evalExperimentAsString (fromMaybe "atlas_driver_offer_bpp_v2" tenant) fp toss
  config' <- jsonToFarePolicy contextValue $ Text.unpack $ getId id
  config <- maybe (findFarePolicyFromCACHelper id fp toss) (pure . Just) config'
  when (isJust txnId) do
    variantIds <- liftIO $ CM.getVariants (fromMaybe "atlas_driver_offer_bpp_v2" tenant) fp toss
    let idName' = fromMaybe (error "idName not found") idName
        cacData = CACData (fromJust txnId) idName' (Text.pack fp) "farePolicy" (Text.pack (show variantIds))
    pushToKafka cacData "cac-data" ""
  pure config

getConfigFromInMemory :: KvDbFlow m r => Id FarePolicy -> Int -> m (Maybe FarePolicy)
getConfigFromInMemory id toss = do
  fp <- L.getOption (DTC.FarePolicy id.getId)
  maybe
    ( findFarePolicyFromCAC id toss Nothing Nothing
        >>= ( \config -> do
                when (isJust config) do
                  L.setOption (DTC.FarePolicy id.getId) (fromJust config)
                pure config
            )
    )
    ( \config' -> do
        isUpdateReq <- DTC.updateConfig DTC.LastUpdatedFarePolicy
        if isUpdateReq
          then do
            config <- findFarePolicyFromCAC id toss Nothing Nothing
            when (isJust config) do
              L.setOption (DTC.FarePolicy id.getId) (fromJust config)
            pure config
          else pure $ Just config'
    )
    fp

findById :: KvDbFlow m r => Maybe Text -> Maybe Text -> Id FarePolicy -> m (Maybe FarePolicy)
findById txnId idName id = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  config <-
    ( if "fare_policy" `GL.elem` useCACConfig
        then
          ( do
              logDebug $ "Getting farePolicy from CAC for farePolicyId:" <> getId id
              tenant <- liftIO $ SE.lookupEnv "TENANT"
              isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "atlas_driver_offer_bpp_v2" tenant)
              if isExp && isJust txnId
                then do
                  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACFarePolicy (fromJust txnId)) >>= \case
                    Just (a :: Int) -> do
                      findFarePolicyFromCAC id a txnId idName
                    Nothing -> do
                      gen <- newStdGen
                      let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
                      _ <- cacheToss (fromJust txnId) toss
                      findFarePolicyFromCAC id toss txnId idName
                else getConfigFromInMemory id 1
          )
        else
          ( do
              logDebug $ "Getting farePolicy from DB for farePolicyId:" <> getId id
              findFarePolicyFromDB id
          )
      )
  logDebug $ "farePlicy we recieved for farePolicyId:" <> getId id <> " is:" <> show config
  pure config

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime

cacheToss :: (CacheFlow m r) => Text -> Int -> m ()
cacheToss txnId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACFarePolicy txnId) toss expTime

makeCACFarePolicy :: Text -> Text
makeCACFarePolicy id = "driver-offer:CAC:CachedQueries-" <> id

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)

clearCacheById :: HedisFlow m r => Id FarePolicy -> m ()
clearCacheById fid = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fid)

update :: KvDbFlow m r => FarePolicy -> m ()
update = Queries.update

update' :: KvDbFlow m r => FarePolicy -> m ()
update' = Queries.update'
