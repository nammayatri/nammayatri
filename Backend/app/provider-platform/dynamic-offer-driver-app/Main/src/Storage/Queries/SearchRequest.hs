{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequest where

import qualified Domain.Types.LocationMapping as LM
import Domain.Types.SearchRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders
import Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.SearchRequest

create :: SearchRequest -> [LM.LocationMapping] -> SqlDB ()
create dsReq mappings = do
  Esq.runTransaction $
    withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
      Esq.create' sReq
      void $ Esq.createUnique' fromLoc
      traverse_ Esq.createUnique' toLoc
  QLocationMapping.createMany mappings

findById ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe SearchRequest)
findById searchRequestId = buildDType $ do
  res <- findOne' $ do
    searchRequestT <- from $ table @SearchRequestT
    where_ $
      searchRequestT ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequestT
  join <$> mapM buildFullSearchRequest res

findByTransactionId ::
  (Transactionable m) =>
  Text ->
  m (Maybe (Id SearchRequest))
findByTransactionId transactionId = do
  findOne $ do
    searchReqT <- from $ table @SearchRequestT
    where_ $
      searchReqT ^. SearchRequestTransactionId ==. val transactionId
    return $ searchReqT ^. SearchRequestTId

updateAutoAssign ::
  Id SearchRequest ->
  Bool ->
  SqlDB ()
updateAutoAssign searchRequestId autoAssignedEnabled = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestAutoAssignEnabled =. val (Just autoAssignedEnabled)
      ]
    where_ $ tbl ^. SearchRequestTId ==. val (toKey searchRequestId)
