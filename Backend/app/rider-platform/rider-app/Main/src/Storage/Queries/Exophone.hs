{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Exophone
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Exophone
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Exophone

create :: Exophone -> SqlDB ()
create = Esq.create

findAllByPhone :: Transactionable m => Text -> m [Exophone]
findAllByPhone phone = do
  findAll $ do
    exophone <- from $ table @ExophoneT
    where_ $ just (exophone ^. ExophoneMerchantId) ==. subSelect subQuery
    return exophone
  where
    subQuery = do
      exophone1 <- from $ table @ExophoneT
      where_ $
        exophone1 ^. ExophonePrimaryPhone ==. val phone
          ||. exophone1 ^. ExophoneBackupPhone ==. val phone
      return (exophone1 ^. ExophoneMerchantId)

findAllByMerchantId :: Transactionable m => Id DM.Merchant -> m [Exophone]
findAllByMerchantId merchantId = do
  findAll $ do
    exophone <- from $ table @ExophoneT
    where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)
    return exophone

findAllExophones :: Transactionable m => m [Exophone]
findAllExophones = findAll $ from $ table @ExophoneT

updateAffectedPhones :: [Text] -> SqlDB ()
updateAffectedPhones primaryPhones = do
  now <- getCurrentTime
  let primaryPhonesList = valList primaryPhones
  Esq.update $ \tbl -> do
    let isPrimaryDown = tbl ^. ExophonePrimaryPhone `in_` primaryPhonesList
    set
      tbl
      [ ExophoneIsPrimaryDown =. isPrimaryDown,
        ExophoneUpdatedAt =. val now
      ]
    where_ $ isPrimaryDown !=. tbl ^. ExophoneIsPrimaryDown

deleteByMerchantId :: Id DM.Merchant -> SqlDB ()
deleteByMerchantId merchantId = do
  Esq.delete $ do
    exophone <- from $ table @ExophoneT
    where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)
