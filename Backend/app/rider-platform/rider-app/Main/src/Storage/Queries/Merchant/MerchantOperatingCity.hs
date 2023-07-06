{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.MerchantOperatingCity
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
-- import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Merchant.MerchantOperatingCity
  ( EntityField
      ( MerchantOperatingCityCity,
        MerchantOperatingCityMerchantId
      ),
    MerchantOperatingCityT,
  )

findByMerchantIdAndCity :: Transactionable m => Id DM.Merchant -> City -> m (Maybe MerchantOperatingCity)
findByMerchantIdAndCity merchantId city =
  Esq.findOne $ do
    merchantOperatingCity <- from $ table @MerchantOperatingCityT
    where_ $
      merchantOperatingCity ^. MerchantOperatingCityMerchantId ==. val (toKey merchantId)
        &&. merchantOperatingCity ^. MerchantOperatingCityCity ==. val city
    return merchantOperatingCity

findByMerchantId :: Transactionable m => Id DM.Merchant -> m (Maybe MerchantOperatingCity)
findByMerchantId merchantId =
  Esq.findOne $ do
    merchantOperatingCity <- from $ table @MerchantOperatingCityT
    where_ $
      merchantOperatingCity ^. MerchantOperatingCityMerchantId ==. val (toKey merchantId)
    return merchantOperatingCity
