{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Merchant

findById :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m (Maybe Merchant)
findById merchatId _ = Esq.findById @m @ma merchatId

findByShortId :: forall m ma. Transactionable ma m => ShortId Merchant -> Proxy ma -> m (Maybe Merchant)
findByShortId shortId_ _ = do
  findOne @m @ma $ do
    merchant <- from $ table @MerchantT
    where_ $ merchant ^. MerchantShortId ==. val (getShortId shortId_)
    return merchant

findByExoPhone :: forall m ma. Transactionable ma m => Text -> Text -> Proxy ma -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone _ = do
  findOne @m @ma $ do
    merchant <- from $ table @MerchantT
    where_ $
      merchant ^. MerchantExoPhoneCountryCode ==. val (Just countryCode)
        &&. merchant ^. MerchantExoPhone ==. val (Just exoPhone)
    return merchant

findAll :: forall m ma. Transactionable ma m => Proxy ma -> m [Merchant]
findAll _ =
  Esq.findAll @m @ma $ do from $ table @MerchantT

update :: Merchant -> SqlDB m ()
update merchant = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantName =. val merchant.name,
        MerchantFcmUrl =. val (showBaseUrl merchant.fcmConfig.fcmUrl),
        MerchantFcmServiceAccount =. val merchant.fcmConfig.fcmServiceAccount,
        MerchantExoPhone =. val merchant.exoPhone,
        MerchantExoPhoneCountryCode =. val merchant.exoPhoneCountryCode,
        MerchantGatewayUrl =. val (showBaseUrl merchant.gatewayUrl),
        MerchantRegistryUrl =. val (showBaseUrl merchant.registryUrl),
        MerchantUpdatedAt =. val now
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey merchant.id)
