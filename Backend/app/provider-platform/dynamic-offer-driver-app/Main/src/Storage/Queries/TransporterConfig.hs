{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.TransporterConfig
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.TransporterConfig

findByMerchantId :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m (Maybe TransporterConfig)
findByMerchantId merchantId _ =
  Esq.findOne @m @ma $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigMerchantId ==. val (toKey merchantId)
    return config

updateFCMConfig :: Id Merchant -> BaseUrl -> Text -> SqlDB m ()
updateFCMConfig merchantId fcmUrl fcmServiceAccount = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TransporterConfigFcmUrl =. val (showBaseUrl fcmUrl),
        TransporterConfigFcmServiceAccount =. val fcmServiceAccount,
        TransporterConfigUpdatedAt =. val now
      ]
    where_ $ tbl ^. TransporterConfigMerchantId ==. val (toKey merchantId)
