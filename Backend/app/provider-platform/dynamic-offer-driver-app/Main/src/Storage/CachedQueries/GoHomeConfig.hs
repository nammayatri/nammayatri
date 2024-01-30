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
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import qualified Storage.Queries.GoHomeConfig as Queries
import qualified System.Environment as SE
import Tools.Error (GenericError (..))

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> m GoHomeConfig
findByMerchantOpCityId id = do
  ghcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id))]
  logDebug $ "the context is " <> show ghcCond
  tenant <- liftIO $ SE.lookupEnv "DRIVER_TENANT"
  contextValue <- liftIO $ CM.evalCtx (fromMaybe "atlas_driver_offer_bpp_v2" tenant) ghcCond
  case contextValue of
    Left err -> error $ (pack "error in fetching the context value for GoHomeConfig ") <> (pack err)
    Right contextValue' -> do
      logDebug $ "the fetched context value is :" <> show contextValue'
      --value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
      valueHere <- buildGhcType contextValue'
      logDebug $ "the build context value is : " <> show valueHere
      return valueHere
  where
    buildGhcType cv =
      case (DAT.parse jsonToGoHomeConfig cv) of
        Success ghc -> pure ghc
        Error err -> do
          logError $ (pack "error in parsing the context value for GoHomeConfig : ") <> (pack err)
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.safeGet (makeGoHomeKey id) >>= \case
            Just cfg -> return cfg
            Nothing -> do
              cfg <- fromMaybeM (InternalError ("Could not find Go-To config corresponding to the stated merchant id" <> show id)) =<< Queries.findByMerchantOpCityId id
              Hedis.setExp (makeGoHomeKey id) cfg expTime
              return cfg

makeGoHomeKey :: Id MerchantOperatingCity -> Text
makeGoHomeKey id = "driver-offer:CachedQueries:GoHomeConfig:MerchantOpCityId-" <> id.getId
