{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Merchant where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

findMerchantByShortId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => ShortId DM.Merchant -> m DM.Merchant
findMerchantByShortId merchantShortId = do
  CQM.findByShortId merchantShortId
    >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

findById :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DM.Merchant -> m DM.Merchant
findById merchantId = do
  CQM.findById merchantId
    >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)

getCurrencyByMerchantOpCity :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m Currency
getCurrencyByMerchantOpCity merchantOpCityId = do
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  pure merchantOperatingCity.currency

checkCurrencies :: (MonadThrow m, Log m) => Currency -> [Maybe PriceAPIEntity] -> m ()
checkCurrencies currency fields =
  unless (all (\field -> field.currency == currency) (catMaybes fields)) $
    throwError (InvalidRequest "Invalid currency")

getDistanceUnitByMerchantOpCity :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DistanceUnit
getDistanceUnitByMerchantOpCity merchantOpCityId = do
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  pure merchantOperatingCity.distanceUnit
