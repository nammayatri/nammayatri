{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IGMIssue where

import qualified Data.Text
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
      Set Beam.respondentName respondentName,
      Set Beam.respondentEmail respondentEmail,
      Set Beam.respondentPhone respondentPhone,
      Set Beam.resolutionShortDesc resolutionShortDesc,
      Set Beam.resolutionLongDesc resolutionLongDesc,
      Set Beam.resolutionActionTriggered resolutionActionTriggered,
      Set Beam.resolutionRefundAmount resolutionRefundAmount,
      Set Beam.descriptionShort descriptionShort,
      Set Beam.descriptionLong descriptionLong,
      Set Beam.orderState orderState,
      Set Beam.orderProviderId orderProviderId,
      Set Beam.orderMerchantOrderId orderMerchantOrderId,
      Set Beam.orderItemId orderItemId,
      Set Beam.orderItemQuantity orderItemQuantity,
      Set Beam.orderFulfillmentId orderFulfillmentId,
      Set Beam.orderFulfillmentState orderFulfillmentState,
      Set Beam.igmCategory igmCategory,
      Set Beam.igmSubCategory igmSubCategory,
      Set Beam.sourceType sourceType,
      Set Beam.sourceNpId sourceNpId,
      Set Beam.contextTransactionId contextTransactionId,
      Set Beam.contextDomain contextDomain,
      Set Beam.expectedResponseTime expectedResponseTime,
      Set Beam.expectedResolutionTime expectedResolutionTime,
      Set Beam.issueRating issueRating,
      Set Beam.updatedAt _now
    ]
    [And [Is Beam.id $ Eq (getId id)]]

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
            descriptionShort = descriptionShort,
            descriptionLong = descriptionLong,
            orderState = orderState,
            orderProviderId = orderProviderId,
            orderMerchantOrderId = orderMerchantOrderId,
            orderItemId = orderItemId,
            orderItemQuantity = orderItemQuantity,
            orderFulfillmentId = orderFulfillmentId,
            orderFulfillmentState = orderFulfillmentState,
            igmCategory = igmCategory,
            igmSubCategory = igmSubCategory,
            sourceType = sourceType,
            sourceNpId = sourceNpId,
            contextTransactionId = contextTransactionId,
            contextDomain = contextDomain,
            expectedResponseTime = expectedResponseTime,
            expectedResolutionTime = expectedResolutionTime,
            issueRating = issueRating,
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
        Beam.resolutionShortDesc = resolutionShortDesc,
        Beam.resolutionLongDesc = resolutionLongDesc,
        Beam.resolutionActionTriggered = resolutionActionTriggered,
        Beam.resolutionRefundAmount = resolutionRefundAmount,
        Beam.descriptionShort = descriptionShort,
        Beam.descriptionLong = descriptionLong,
        Beam.orderState = orderState,
        Beam.orderProviderId = orderProviderId,
        Beam.orderMerchantOrderId = orderMerchantOrderId,
        Beam.orderItemId = orderItemId,
        Beam.orderItemQuantity = orderItemQuantity,
        Beam.orderFulfillmentId = orderFulfillmentId,
        Beam.orderFulfillmentState = orderFulfillmentState,
        Beam.igmCategory = igmCategory,
        Beam.igmSubCategory = igmSubCategory,
        Beam.sourceType = sourceType,
        Beam.sourceNpId = sourceNpId,
        Beam.contextTransactionId = contextTransactionId,
        Beam.contextDomain = contextDomain,
        Beam.expectedResponseTime = expectedResponseTime,
        Beam.expectedResolutionTime = expectedResolutionTime,
        Beam.issueRating = issueRating,
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
        Beam.updatedAt = updatedAt
      }
