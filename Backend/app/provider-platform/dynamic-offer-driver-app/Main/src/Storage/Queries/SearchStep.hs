{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchStep where

import Domain.Types.SearchStep as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.SearchRequest.SearchReqLocation
import Storage.Tabular.SearchStep

create :: SearchStep -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: Transactionable m => Id SearchStep -> m (Maybe SearchStep)
findById searchRequestId = buildDType $
  fmap (fmap $ extractSolidType @Domain.SearchStep) $
    Esq.findOne' $ do
      (sReq :& sFromLoc :& sToLoc) <-
        from
          ( table @SearchStepT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchStepFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchStepToLocationId ==. loc2 ^. SearchReqLocationTId)
          )
      where_ $ sReq ^. SearchStepTId ==. val (toKey searchRequestId)
      pure (sReq, sFromLoc, sToLoc)

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

getRequestIdfromTransactionId ::
  (Transactionable m) =>
  Id SearchStep ->
  m (Maybe (Id SearchStep))
getRequestIdfromTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchStepT
    where_ $
      searchT ^. SearchStepTransactionId ==. val (getId tId)
    return $ searchT ^. SearchStepTId

getStatus ::
  (Transactionable m) =>
  Id SearchStep ->
  m (Maybe SearchStepStatus)
getStatus searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchStepT
    where_ $
      searchT ^. SearchStepTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchStepStatus

getValidTill ::
  (Transactionable m) =>
  Id SearchStep ->
  m (Maybe UTCTime)
getValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchStepT
    where_ $
      searchT ^. SearchStepTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchStepValidTill
