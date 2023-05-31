{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchTry where

import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.SearchTry

create :: SearchTry -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id SearchTry -> m (Maybe SearchTry)
findById = Esq.findById

cancelActiveTriesByRequestId ::
  Id SearchRequest ->
  SqlDB ()
cancelActiveTriesByRequestId searchId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchTryUpdatedAt =. val now,
        SearchTryStatus =. val CANCELLED
      ]
    where_ $
      tbl ^. SearchTryRequestId ==. val (toKey searchId)
        &&. tbl ^. SearchTryStatus ==. val ACTIVE

updateStatus ::
  Id SearchTry ->
  SearchTryStatus ->
  SqlDB ()
updateStatus searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchTryUpdatedAt =. val now,
        SearchTryStatus =. val status_
      ]
    where_ $ tbl ^. SearchTryTId ==. val (toKey searchId)

getSearchTryStatusAndValidTill ::
  (Transactionable m) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchTryT
    where_ $
      searchT ^. SearchTryTId ==. val (toKey searchRequestId)
    return (searchT ^. SearchTryValidTill, searchT ^. SearchTryStatus)
