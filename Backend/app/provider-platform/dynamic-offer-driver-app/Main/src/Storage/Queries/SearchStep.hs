{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchStep where

import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchStep as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.SearchStep

create :: SearchStep -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id SearchStep -> m (Maybe SearchStep)
findById = Esq.findById

cancelActiveStepsByRequestId ::
  Id SearchRequest ->
  SqlDB ()
cancelActiveStepsByRequestId searchId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchStepUpdatedAt =. val now,
        SearchStepStatus =. val CANCELLED
      ]
    where_ $
      tbl ^. SearchStepRequestId ==. val (toKey searchId)
        &&. tbl ^. SearchStepStatus ==. val ACTIVE

updateStatus ::
  Id SearchStep ->
  SearchStepStatus ->
  SqlDB ()
updateStatus searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchStepUpdatedAt =. val now,
        SearchStepStatus =. val status_
      ]
    where_ $ tbl ^. SearchStepTId ==. val (toKey searchId)

getStatus ::
  (Transactionable m) =>
  Id SearchStep ->
  m (Maybe SearchStepStatus)
getStatus searchRequestId = do
  Esq.findOne $ do
    searchT <- from $ table @SearchStepT
    where_ $
      searchT ^. SearchStepTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchStepStatus

getValidTill ::
  (Transactionable m) =>
  Id SearchStep ->
  m (Maybe UTCTime)
getValidTill searchRequestId = do
  Esq.findOne $ do
    searchT <- from $ table @SearchStepT
    where_ $
      searchT ^. SearchStepTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchStepValidTill
