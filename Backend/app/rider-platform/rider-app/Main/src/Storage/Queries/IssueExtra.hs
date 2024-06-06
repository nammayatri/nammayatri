module Storage.Queries.IssueExtra where

-- Extra code goes here --
import Domain.Types.Booking as Booking
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
import Storage.Queries.OrphanInstances.Issue ()
import qualified Storage.Queries.Person ()

updateIssueStatus :: (MonadFlow m, EsqDBFlow m r) => Text -> Domain.IssueStatus -> m () -- can only send maybe text
updateIssueStatus ticketId status = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamI.status status, Se.Set BeamI.updatedAt now]
    [Se.Is BeamI.ticketId (Se.Eq (Just ticketId))]

updateTicketId :: (MonadFlow m, EsqDBFlow m r) => Id Issue -> Text -> m ()
updateTicketId issueId ticketId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamI.ticketId (Just ticketId), Se.Set BeamI.updatedAt now]
    [Se.Is BeamI.id (Se.Eq $ getId issueId)]

findByTicketId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Issue)
findByTicketId ticketId = findOneWithKV [Se.Is BeamI.ticketId $ Se.Eq (Just ticketId)]

findNightIssueByBookingId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> m (Maybe Issue)
findNightIssueByBookingId (Id bookingId) = findOneWithKV [Se.And [Se.Is BeamI.bookingId $ Se.Eq (Just bookingId), Se.Is BeamI.nightSafety $ Se.Eq True]] -- we are storing bookingId as quoteId

findByCustomerId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [(Issue, Person)] -- big queries
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
       in acc <> ((issue,) <$> persons')

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
       in acc <> ((issue,) <$> persons')
