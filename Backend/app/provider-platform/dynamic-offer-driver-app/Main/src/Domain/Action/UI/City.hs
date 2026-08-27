{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.City where

import qualified Domain.Types.City as DTC
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity (..))
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Tools.Auth

listCities :: Id DM.Merchant -> Flow [DTC.CityRes]
listCities mId = do
  let merchantId = merchantIdFallback mId
  merchantOperatingCities <-
    CQMOC.findAllByMerchantId merchantId >>= \case
      [] -> CQMOC.findAllByMerchantShortId (ShortId merchantId.getId)
      cities -> pure cities
  mapM mkCityRes merchantOperatingCities
  where
    mkCityRes MerchantOperatingCity {..} = do
      mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = id.getId}) Nothing
      return $
        DTC.CityRes
          { code = city,
            name = show city,
            countryDialCode = fromMaybe "+91" countryDialCode,
            subscription = maybe False (.subscription) mbTransporterConfig,
            ..
          }

listCityMerchants :: Context.City -> Flow [DTC.CityMerchantRes]
listCityMerchants reqCity = do
  merchantOperatingCities <- CQMOC.findAllByCity reqCity
  catMaybes <$> mapM mkCityMerchantRes merchantOperatingCities
  where
    mkCityMerchantRes MerchantOperatingCity {..} = do
      mbMerchant <- CQM.findById merchantId
      return $
        mbMerchant >>= \merchant ->
          if not merchant.enabled
            then Nothing
            else
              Just
                DTC.CityMerchantRes
                  { merchantId = merchantId.getId,
                    merchantShortId = merchantShortId.getShortId,
                    merchantName = merchant.name,
                    countryDialCode = fromMaybe "+91" countryDialCode,
                    ..
                  }
