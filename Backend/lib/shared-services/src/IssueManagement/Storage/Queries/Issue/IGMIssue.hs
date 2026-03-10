{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IGMIssue where

import qualified Data.Text
import qualified Data.Time as T
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IGMIssue as DIGMIssue
import qualified IssueManagement.Storage.Beam.Issue.IGMIssue as Beam
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => (DIGMIssue.IGMIssue -> m ())
create = createWithKV

createMany :: BeamFlow m r => ([DIGMIssue.IGMIssue] -> m ())
createMany = traverse_ create

findByPrimaryKey :: BeamFlow m r => (Id DIGMIssue.IGMIssue -> m (Maybe DIGMIssue.IGMIssue))
findByPrimaryKey (Id id) = do findOneWithKV [And [Is Beam.id $ Eq id]]

findAllByRiderIdandStatus :: BeamFlow m r => (Maybe (Id Person) -> DIGMIssue.Status -> m [DIGMIssue.IGMIssue])
findAllByRiderIdandStatus riderId issueStatus = do findAllWithKV [And [Is Beam.riderId $ Eq (getId <$> riderId), Is Beam.issueStatus $ Eq issueStatus]]

findAllByStatus :: BeamFlow m r => (DIGMIssue.Status -> m [DIGMIssue.IGMIssue])
findAllByStatus issueStatus = do findAllWithKV [Is Beam.issueStatus $ Eq issueStatus]

findByTransactionId :: BeamFlow m r => (Data.Text.Text -> m (Maybe DIGMIssue.IGMIssue))
findByTransactionId transactionId = do findOneWithKV [Is Beam.transactionId $ Eq transactionId]

findByBookingId :: BeamFlow m r => (Data.Text.Text -> m [DIGMIssue.IGMIssue])
findByBookingId bookingId = do findAllWithKV [Is Beam.bookingId $ Eq bookingId]

updateByPrimaryKey :: BeamFlow m r => (DIGMIssue.IGMIssue -> m ())
updateByPrimaryKey (DIGMIssue.IGMIssue {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Set Beam.bookingId bookingId,
      Set Beam.createdAt createdAt,
      Set Beam.customerEmail customerEmail,
      Set Beam.customerName customerName,
      Set Beam.customerPhone customerPhone,
      Set Beam.issueRaisedByMerchant issueRaisedByMerchant,
      Set Beam.issueStatus issueStatus,
      Set Beam.issueType issueType,
      Set Beam.merchantId (getId <$> merchantId),
      Set Beam.respondentAction respondentAction,
      Set Beam.resolutionShortDesc resolutionShortDesc,
      Set Beam.resolutionLongDesc resolutionLongDesc,
      Set Beam.resolutionActionTriggered resolutionActionTriggered,
      Set Beam.resolutionRefundAmount resolutionRefundAmount,
      Set Beam.updatedAt _now
    ]
    [And [Is Beam.id $ Eq (getId id)]]

-- Dashboard queries: list IGM issues where merchantId matches (issues raised against this merchant)
findAllByMerchantIdWithOptions ::
  BeamFlow m r =>
  Id Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe DIGMIssue.Status ->
  Maybe T.UTCTime ->
  Maybe T.UTCTime ->
  m (Int, [DIGMIssue.IGMIssue])
findAllByMerchantIdWithOptions (Id mId) mbLimit mbOffset mbStatus mbFromDate mbToDate = do
  let conditions =
        [ And $
            catMaybes
              [ Just $ Is Beam.merchantId $ Eq (Just mId),
                (Is Beam.issueStatus . Eq) <$> mbStatus,
                (\fromDate -> Is Beam.createdAt $ GreaterThanOrEq fromDate) <$> mbFromDate,
                (\toDate -> Is Beam.createdAt $ LessThanOrEq toDate) <$> mbToDate
              ]
        ]
  issues <- findAllWithOptionsKV conditions (Desc Beam.createdAt) (Just limitVal) (Just offsetVal)
  return (length issues, issues)
  where
    limitVal = min (fromMaybe 10 mbLimit) 50
    offsetVal = fromMaybe 0 mbOffset

-- Dashboard queries: list IGM issues raised by a specific merchant (issues this merchant raised against others)
findAllByIssueRaisedByMerchantWithOptions ::
  BeamFlow m r =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe DIGMIssue.Status ->
  Maybe T.UTCTime ->
  Maybe T.UTCTime ->
  m (Int, [DIGMIssue.IGMIssue])
findAllByIssueRaisedByMerchantWithOptions raisedByMerchant mbLimit mbOffset mbStatus mbFromDate mbToDate = do
  let conditions =
        [ And $
            catMaybes
              [ Just $ Is Beam.issueRaisedByMerchant $ Eq (Just raisedByMerchant),
                (Is Beam.issueStatus . Eq) <$> mbStatus,
                (\fromDate -> Is Beam.createdAt $ GreaterThanOrEq fromDate) <$> mbFromDate,
                (\toDate -> Is Beam.createdAt $ LessThanOrEq toDate) <$> mbToDate
              ]
        ]
  issues <- findAllWithOptionsKV conditions (Desc Beam.createdAt) (Just limitVal) (Just offsetVal)
  return (length issues, issues)
  where
    limitVal = min (fromMaybe 10 mbLimit) 50
    offsetVal = fromMaybe 0 mbOffset

-- Update resolution fields on an IGM issue
updateResolution :: BeamFlow m r => Id DIGMIssue.IGMIssue -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m ()
updateResolution (Id igmId) shortDesc longDesc actionTriggered refundAmount = do
  _now <- getCurrentTime
  updateWithKV
    [ Set Beam.resolutionShortDesc shortDesc,
      Set Beam.resolutionLongDesc longDesc,
      Set Beam.resolutionActionTriggered actionTriggered,
      Set Beam.resolutionRefundAmount refundAmount,
      Set Beam.updatedAt _now
    ]
    [And [Is Beam.id $ Eq igmId]]

instance FromTType' Beam.IGMIssue DIGMIssue.IGMIssue where
  fromTType' (Beam.IGMIssueT {..}) = do
    pure $
      Just
        DIGMIssue.IGMIssue
          { bookingId = bookingId,
            createdAt = createdAt,
            customerEmail = customerEmail,
            riderId = Id <$> riderId,
            respondingMerchantId = respondingMerchantId,
            respondentEntityType = respondentEntityType,
            resolutionShortDesc = resolutionShortDesc,
            resolutionLongDesc = resolutionLongDesc,
            resolutionActionTriggered = resolutionActionTriggered,
            resolutionRefundAmount = resolutionRefundAmount,
            transactionId = transactionId,
            domain = mkStringToDomainType domain,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            customerName = customerName,
            customerPhone = customerPhone,
            id = Id id,
            issueRaisedByMerchant = issueRaisedByMerchant,
            issueStatus = issueStatus,
            issueType = issueType,
            merchantId = Id <$> merchantId,
            respondentName = respondentName,
            respondentEmail = respondentEmail,
            respondentPhone = respondentPhone,
            respondentAction = respondentAction,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IGMIssue DIGMIssue.IGMIssue where
  toTType' (DIGMIssue.IGMIssue {..}) = do
    Beam.IGMIssueT
      { Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.customerEmail = customerEmail,
        Beam.riderId = getId <$> riderId,
        Beam.respondingMerchantId = respondingMerchantId,
        Beam.respondentEntityType = respondentEntityType,
        Beam.transactionId = transactionId,
        Beam.domain = show domain,
        Beam.merchantOperatingCityId = getId <$> merchantOperatingCityId,
        Beam.customerName = customerName,
        Beam.customerPhone = customerPhone,
        Beam.id = getId id,
        Beam.issueRaisedByMerchant = issueRaisedByMerchant,
        Beam.issueStatus = issueStatus,
        Beam.issueType = issueType,
        Beam.merchantId = getId <$> merchantId,
        Beam.respondentEmail = respondentEmail,
        Beam.respondentPhone = respondentPhone,
        Beam.respondentName = respondentName,
        Beam.respondentAction = respondentAction,
        Beam.resolutionShortDesc = resolutionShortDesc,
        Beam.resolutionLongDesc = resolutionLongDesc,
        Beam.resolutionActionTriggered = resolutionActionTriggered,
        Beam.resolutionRefundAmount = resolutionRefundAmount,
        Beam.updatedAt = updatedAt
      }
