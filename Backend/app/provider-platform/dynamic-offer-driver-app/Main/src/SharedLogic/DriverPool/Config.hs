{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Client.Main as CM
import Data.Aeson as DA
-- import Domain.Types.Merchant.DriverPoolConfig as DPC

-- import Data.Aeson.Key
-- import qualified Data.Time.Clock as DTC
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text.Encoding as DTE
import Data.Aeson.Types as DAT
import Data.HashMap.Strict as HashMap
import Data.Text as Text hiding (find)
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import qualified System.Environment as SE

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Variant.Variant ->
  Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId mbvt dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId)), (pack "tripDistance", DA.String (Text.pack (show dist)))] ++ (bool [] [(pack "variant", DA.String (Text.pack (show $ fromJust mbvt)))] (isJust mbvt)))
  logDebug $ "the context value is " <> show dpcCond
  tenant <- liftIO $ SE.lookupEnv "DRIVER_TENANT"
  contextValue <- liftIO $ CM.evalCtx (fromMaybe "atlas_driver_offer_bpp_v2" tenant) dpcCond
  case contextValue of
    Left err -> error $ (pack "error in fetching the context value ") <> (pack err)
    Right contextValue' -> do
      logDebug $ "the fetched context value is " <> show contextValue'
      --value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
      valueHere <- buildDpcType contextValue'
      logDebug $ "the build context value is1 " <> show valueHere
      return valueHere
  where
    buildDpcType cv =
      case (DAT.parse jsonToDriverPoolConfig cv) of
        Success dpc -> pure $ dpc
        Error err -> do
          logError $ (pack "error in parsing the context value ") <> (pack err)

          configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
          case mbvt of
            Nothing -> getDefaultDriverPoolConfig configs dist
            (Just vehicle) -> do
              let mbApplicableConfig = find (filterByDistAndDveh (Just vehicle) dist) configs
              case configs of
                [] -> throwError $ InvalidRequest "DriverPoolConfig not found"
                _ ->
                  case mbApplicableConfig of
                    Just applicableConfig -> return applicableConfig
                    Nothing -> getDefaultDriverPoolConfig configs dist

filterByDistAndDveh :: Maybe Variant.Variant -> Meters -> DriverPoolConfig -> Bool
filterByDistAndDveh mbVehicle_ dist cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == mbVehicle_

getDefaultDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Meters -> m DriverPoolConfig
getDefaultDriverPoolConfig configs dist = do
  find (filterByDistAndDveh Nothing dist) configs
    & fromMaybeM (InvalidRequest "DriverPool default config not found")
