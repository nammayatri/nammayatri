{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BlackListOrg
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.BlackListOrg
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Registry.Subscriber (Subscriber)
import Storage.Tabular.BlackListOrg

findBySubscriberId :: Transactionable m => ShortId Subscriber -> m (Maybe BlackListOrg)
findBySubscriberId subscriberId = do
  findOne $ do
    org <- from $ table @BlackListOrgT
    where_ $ org ^. BlackListOrgSubscriberId ==. val (getShortId subscriberId)
    return org
