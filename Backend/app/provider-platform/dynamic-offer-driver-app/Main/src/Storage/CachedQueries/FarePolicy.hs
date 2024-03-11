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
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.FarePolicy as Queries
import qualified System.Environment as SE
import System.Random

findFarePolicyFromCAC :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> Int -> m (Maybe FarePolicy)
findFarePolicyFromCAC id toss = do
  fp <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "farePolicyId", DA.String (getId id))]
  tenant <- liftIO $ SE.lookupEnv "TENANT"
  contextValue <- liftIO $ CM.evalExperimentAsString (fromMaybe "test" tenant) fp toss
  let config = jsonToFarePolicy contextValue $ Text.unpack $ getId id
  pure config

getConfigFromInMemory :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> Int -> m (Maybe FarePolicy)
getConfigFromInMemory id toss = do
  fp <- L.getOption DTC.FarePolicy
  maybe
    ( findFarePolicyFromCAC id toss
        >>= ( \config -> do
                L.setOption DTC.FarePolicy (fromJust config)
                pure config
            )
    )
    (pure . Just)
    fp

findById :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Id FarePolicy -> m (Maybe FarePolicy)
findById txnId id = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  ( if "fare_policy" `GL.elem` useCACConfig
      then
        ( do
            tenant <- liftIO $ SE.lookupEnv "TENANT"
            isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
            if isExp && isJust txnId
              then do
                Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACFarePolicy (fromJust txnId)) >>= \case
                  Just (a :: Int) -> do
                    findFarePolicyFromCAC id a
                  Nothing -> do
                    gen <- newStdGen
                    let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
                    logDebug $ "the toss value is for farePolicy " <> show toss
                    _ <- cacheToss (fromJust txnId) toss
                    getConfigFromInMemory id toss
              else findFarePolicyFromCAC id 1
        )
      else
        ( do
            Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
              Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
              Nothing -> do
                flip whenJust cacheFarePolicy /=<< Queries.findById id
        )
    )

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
makeCACFarePolicy id = "driver-offer:CAC:CachedQueries:TransporterConfig:PersonId-" <> id

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)

clearCacheById :: HedisFlow m r => Id FarePolicy -> m ()
clearCacheById fid = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fid)

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FarePolicy -> m ()
update = Queries.update

update' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FarePolicy -> m ()
update' = Queries.update'
