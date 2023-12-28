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
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity (..))
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQMTC
import Tools.Error

listCities :: Id DM.Merchant -> Flow [DTC.CityRes]
listCities mId = do
  merchantOperatingCities <- CQMOC.findAllByMerchantId mId
  mapM mkCityRes merchantOperatingCities
  where
    mkCityRes MerchantOperatingCity {..} = do
      transporterConfig <- CQMTC.findByMerchantOpCityId id >>= fromMaybeM (TransporterConfigNotFound id.getId)
      return $
        DTC.CityRes
          { code = city,
            name = show city,
            subscription = transporterConfig.subscription,
            ..
          }
