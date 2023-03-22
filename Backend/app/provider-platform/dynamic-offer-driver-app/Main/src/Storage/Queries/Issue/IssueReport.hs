{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Issue.IssueReport where

import Domain.Types.Issue.IssueReport
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Storage.Tabular.Issue.IssueReport

create :: IssueReport -> SqlDB ()
create = Esq.create

findAll :: Transactionable m => m [IssueReport]
findAll = Esq.findAll $ do
  from $ table @IssueReportT

findAllByDriver :: Id SP.Person -> Transactionable m => m [IssueReport]
findAllByDriver driverId = Esq.findAll $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    issueReport ^. IssueReportDriverId ==. val (toKey driverId)
      &&. issueReport ^. IssueReportDeleted ==. val False
  pure issueReport

findAllWithLimitOffsetStatus :: Transactionable m => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe Text -> Maybe Text -> m [IssueReport]
findAllWithLimitOffsetStatus mbLimit mbOffset mbStatus mbCategory mbAssignee = Esq.findAll $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    whenJust_ mbStatus (\statusVal -> issueReport ^. IssueReportStatus ==. val statusVal)
      &&. issueReport ^. IssueReportDeleted ==. val False
      &&. issueReport ^. IssueReportAssignee ==. val mbAssignee
      &&. whenJust_ mbCategory (\category -> issueReport ^. IssueReportCategory ==. val category)
  orderBy [desc $ issueReport ^. IssueReportCreatedAt]
  limit limitVal
  offset offsetVal
  return issueReport
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

updateAsDeleted :: Id IssueReport -> Id SP.Person -> SqlDB ()
updateAsDeleted issueReportId driverId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportDeleted =. val True,
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)
        &&. tbl ^. IssueReportDriverId ==. val (toKey driverId)

updateStatus :: Id IssueReport -> IssueStatus -> SqlDB ()
updateStatus issueReportId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportStatus =. val status,
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateAssignee :: Id IssueReport -> Text -> SqlDB ()
updateAssignee issueReportId assignee = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportAssignee =. just (val assignee),
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateStatusAssignee :: Id IssueReport -> IssueStatus -> Text -> SqlDB ()
updateStatusAssignee issueReportId status assignee = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportStatus =. val status,
        IssueReportAssignee =. just (val assignee),
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateOption :: Id IssueReport -> Text -> SqlDB ()
updateOption issueReportId option = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportOption =. just (val option),
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)
