{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issues where

import Domain.Types.Issue as Issue
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import qualified IssueManagement.Common as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, MonadTime (..), getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue as BeamI
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Queries.Person ()

insertIssue :: MonadFlow m => Issue -> m ()
insertIssue = createWithKV

findByCustomerId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [(Issue, Person)]
findByCustomerId (Id customerId) mbLimit mbOffset fromDate toDate = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  issues <-
    findAllWithOptionsKV
      [ Se.And
          [Se.Is BeamI.customerId $ Se.Eq customerId, Se.Is BeamI.createdAt $ Se.GreaterThanOrEq fromDate, Se.Is BeamI.createdAt $ Se.LessThanOrEq toDate]
      ]
      (Se.Desc BeamI.createdAt)
      Nothing
      Nothing
  persons <- findAllWithOptionsKV [Se.And [Se.Is BeamP.id $ Se.In $ getId . Issue.customerId <$> issues]] (Se.Desc BeamP.createdAt) Nothing Nothing

  let issueWithPerson = foldl' (getIssueWithPerson persons) [] issues
  pure $ take limitVal (drop offsetVal issueWithPerson)
  where
    getIssueWithPerson persons acc issue =
      let persons' = filter (\p -> p.id == issue.customerId) persons
       in acc <> ((\p -> (issue, p)) <$> persons')

-- Finding issues over non-Id; do it through DB
findAllIssue :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [(Issue, Person)]
findAllIssue (Id merchantId) mbLimit mbOffset fromDate toDate = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  issues <-
    findAllWithDb
      [ Se.And
          [Se.Is BeamI.createdAt $ Se.GreaterThanOrEq fromDate, Se.Is BeamI.createdAt $ Se.LessThanOrEq toDate]
      ]
  persons <- findAllWithKV [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.id $ Se.In $ getId . Issue.customerId <$> issues]]

  let issueWithPerson = foldl' (getIssueWithPerson persons) [] issues
  pure $ take limitVal (drop offsetVal issueWithPerson)
  where
    getIssueWithPerson persons acc issue =
      let persons' = filter (\p -> p.id == issue.customerId) persons
       in acc <> ((\p -> (issue, p)) <$> persons')

instance FromTType' BeamI.Issue Issue where
  fromTType' BeamI.IssueT {..} = do
    pure $
      Just
        Issue
          { id = Id id,
            customerId = Id customerId,
            bookingId = Id <$> bookingId,
            contactEmail = contactEmail,
            reason = reason,
            description = description,
            ticketId = ticketId,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamI.Issue Issue where
  toTType' Issue {..} = do
    BeamI.IssueT
      { BeamI.id = getId id,
        BeamI.customerId = getId customerId,
        BeamI.bookingId = getId <$> bookingId,
        BeamI.contactEmail = contactEmail,
        BeamI.reason = reason,
        BeamI.description = description,
        BeamI.ticketId = ticketId,
        BeamI.status = status,
        BeamI.createdAt = createdAt,
        BeamI.updatedAt = updatedAt
      }

updateIssueStatus :: MonadFlow m => Text -> Domain.IssueStatus -> m ()
updateIssueStatus ticketId status = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamI.status status, Se.Set BeamI.updatedAt now]
    [Se.Is BeamI.ticketId (Se.Eq (Just ticketId))]

updateTicketId :: MonadFlow m => Id Issue -> Text -> m ()
updateTicketId issueId ticketId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamI.ticketId (Just ticketId), Se.Set BeamI.updatedAt now]
    [Se.Is BeamI.id (Se.Eq $ getId issueId)]

findByTicketId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Issue)
findByTicketId ticketId = findOneWithKV [Se.Is BeamI.ticketId $ Se.Eq (Just ticketId)]
