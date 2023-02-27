{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareProduct
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.FareProduct
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.FareProduct

findEnabledByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantId = findEnabledByMerchantIdAndType Nothing

findEnabledByMerchantIdAndType ::
  Transactionable m =>
  Maybe FareProductType ->
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantIdAndType mbType merchantId =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. whenJust_ mbType (\typ -> fareProduct ^. FareProductProductType ==. val typ)
    pure fareProduct

insertIfNotExist ::
  Id Merchant ->
  FareProductType ->
  SqlDB ()
insertIfNotExist merchantId typ = do
  mbFp <- listToMaybe <$> findEnabledByMerchantIdAndType (Just typ) merchantId
  case mbFp of
    Nothing -> insertFareProduct
    Just _ -> pure ()
  where
    insertFareProduct :: SqlDB ()
    insertFareProduct = do
      now <- getCurrentTime
      guid <- Id <$> generateGUIDText
      Esq.create @_ @FareProduct $
        FareProduct
          { id = guid,
            merchantId = merchantId,
            _type = typ,
            createdAt = now
          }

delete ::
  Id Merchant ->
  FareProductType ->
  SqlDB ()
delete merchantId fpType = Esq.delete $ do
  fareProduct <- from $ table @FareProductT
  where_ $
    fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
      &&. fareProduct ^. FareProductProductType ==. val fpType
