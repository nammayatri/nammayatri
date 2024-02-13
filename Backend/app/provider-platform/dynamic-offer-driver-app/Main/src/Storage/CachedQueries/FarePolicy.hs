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
import Data.Aeson.Types as DAT
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy
import EulerHS.Language as L (getOption)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FarePolicy as Queries
import qualified System.Environment as SE

findById :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findById id = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (\sc -> sc.useCAC) systemConfigs
  case useCACConfig of
    True -> do
      fp <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "farePolicyId", DA.String (getId id))])
      logDebug $ "the context value is " <> show fp
      tenant <- liftIO $ SE.lookupEnv "DRIVER_TENANT"
      contextValue <- liftIO $ CM.evalCtx (fromMaybe "test" tenant) fp
      case contextValue of
        Left err -> error $ (pack "error in fetching the context value for farePolicy:-") <> (pack err)
        Right contextValue' -> do
          logDebug $ "the fetched context value is for farePolicy " <> show contextValue'
          valueHere <- buildFPType contextValue' (getId id)
          return valueHere
    False -> do
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
        Nothing -> do
          flip whenJust cacheFarePolicy /=<< Queries.findById id
  where
    buildFPType contextValue id' = do
      case (DAT.parse (jsonToFarePolicy contextValue) (Text.unpack id')) of
        Success a -> return a
        DAT.Error err -> error $ (pack "error in parsing the context value for farepolicy ") <> (pack err)

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime

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
