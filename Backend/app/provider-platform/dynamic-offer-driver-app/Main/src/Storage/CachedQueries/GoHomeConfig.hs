{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.GoHomeConfig where

import qualified Client.Main as CM
import Control.Monad
import Data.Aeson as DA
import Data.Aeson.Types as DAT
import Data.HashMap.Strict as HashMap
import Data.Text
import Domain.Types.GoHomeConfig
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import qualified Storage.Queries.GoHomeConfig as Queries
import qualified System.Environment as SE
import qualified System.Environment as Se
import System.Random
import Tools.Error (GenericError (..))

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = Queries.create

getGoHomeConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m GoHomeConfig
getGoHomeConfig id toss = do
  confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId id))])
  logDebug $ "goHomeConfig Cond: " <> show confCond
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  context' <- liftIO $ CM.evalExperiment (fromMaybe "driver_offer_bpp_v2" tenant) confCond toss
  logDebug $ "goHomeConfig: " <> show context'
  let ans = case context' of
        Left err -> error $ (pack "error in fetching the context value ") <> (pack err)
        Right contextValue' ->
          case (DAT.parse jsonToGoHomeConfig contextValue') of
            Success dpc -> dpc
            DAT.Error err -> error $ (pack "error in parsing the context value for go home config ") <> (pack err)
  -- pure $ Just ans
  logDebug $ "goHomeConfig: " <> show ans
  pure ans

getGoHomeConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m GoHomeConfig
getGoHomeConfigFromDB id = do
  logDebug $ "Fetching goHomeConfig from DB"
  Hedis.safeGet (makeGoHomeKey id) >>= \case
    Just cfg -> return cfg
    Nothing -> do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      cfg <- fromMaybeM (InternalError ("Could not find Go-To config corresponding to the stated merchant id" <> show id)) =<< Queries.findByMerchantOpCityId id
      Hedis.setExp (makeGoHomeKey id) cfg expTime
      return cfg

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe (Id DP.Person) -> m GoHomeConfig
findByMerchantOpCityId id (Just personId) = do
  enableCAC' <- liftIO $ SE.lookupEnv "ENABLE_CAC"
  let enableCAC = fromMaybe True (enableCAC' >>= readMaybe)
  case enableCAC of
    False -> getGoHomeConfigFromDB id
    True -> do
      tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
      isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
      case isExp of
        True -> do
          Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACGoHomeConfigKey personId) >>= \case
            (Just (a :: Int)) -> do
              getGoHomeConfig id a
            Nothing -> do
              gen <- newStdGen
              let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
              logDebug $ "the toss value is for goHomeConfig " <> show toss
              _ <- cacheToss personId toss
              getGoHomeConfig id toss
        False -> getGoHomeConfig id 1
findByMerchantOpCityId id Nothing = do
  enableCAC' <- liftIO $ SE.lookupEnv "ENABLE_CAC"
  let enableCAC = fromMaybe True (enableCAC' >>= readMaybe)
  case enableCAC of
    True -> do
      gen <- newStdGen
      let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
      logDebug $ "the toss value is for goHomeConfig " <> show toss
      getGoHomeConfig id toss
    False -> getGoHomeConfigFromDB id

makeGoHomeKey :: Id MerchantOperatingCity -> Text
makeGoHomeKey id = "driver-offer:CachedQueries:GoHomeConfig:MerchantOpCityId-" <> id.getId

cacheToss :: (CacheFlow m r) => Id Person -> Int -> m ()
cacheToss personId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACGoHomeConfigKey personId) toss expTime

makeCACGoHomeConfigKey :: Id Person -> Text
makeCACGoHomeConfigKey id = "driver-offer:CAC:CachedQueries:GoHomeConfig:PersonId-" <> id.getId
