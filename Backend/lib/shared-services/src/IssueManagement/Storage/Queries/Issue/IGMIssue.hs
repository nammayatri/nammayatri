{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module IssueManagement.Storage.Queries.Issue.IGMIssue where

import qualified Data.Time as T
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IGMIssue as DIGMIssue
import qualified IssueManagement.Storage.Beam.Issue.IGMIssue as Beam
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import qualified Kernel.Types.Id

create :: BeamFlow m r => (DIGMIssue.IGMIssue -> m ())
create = createWithKV

createMany :: BeamFlow m r => ([DIGMIssue.IGMIssue] -> m ())
createMany = traverse_ create

findByinternalIssueId :: BeamFlow m r => (Kernel.Types.Id.Id DIGMIssue.IGMIssue -> m (Maybe DIGMIssue.IGMIssue))
findByinternalIssueId (Kernel.Types.Id.Id id) = do findOneWithKV [Is Beam.id $ Eq id]

findByPrimaryKey :: BeamFlow m r => (Kernel.Types.Id.Id DIGMIssue.IGMIssue -> m (Maybe DIGMIssue.IGMIssue))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [And [Is Beam.id $ Eq id]]

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
      Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Set Beam.resolutionAction resolutionAction,
      Set Beam.respondentAction respondentAction,
      Set Beam.updatedAt _now
    ]
    [And [Is Beam.id $ Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IGMIssue DIGMIssue.IGMIssue where
  fromTType' (Beam.IGMIssueT {..}) = do
    pure $
      Just
        DIGMIssue.IGMIssue
          { bookingId = bookingId,
            createdAt = createdAt,
            customerEmail = customerEmail,
            customerName = customerName,
            customerPhone = customerPhone,
            id = Kernel.Types.Id.Id id,
            issueRaisedByMerchant = issueRaisedByMerchant,
            issueStatus = issueStatus,
            issueType = issueType,
            merchantId = Kernel.Types.Id.Id merchantId,
            resolutionAction = resolutionAction,
            respondentAction = respondentAction,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IGMIssue DIGMIssue.IGMIssue where
  toTType' (DIGMIssue.IGMIssue {..}) = do
    Beam.IGMIssueT
      { Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.customerEmail = customerEmail,
        Beam.customerName = customerName,
        Beam.customerPhone = customerPhone,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.issueRaisedByMerchant = issueRaisedByMerchant,
        Beam.issueStatus = issueStatus,
        Beam.issueType = issueType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.resolutionAction = resolutionAction,
        Beam.respondentAction = respondentAction,
        Beam.updatedAt = updatedAt
      }
