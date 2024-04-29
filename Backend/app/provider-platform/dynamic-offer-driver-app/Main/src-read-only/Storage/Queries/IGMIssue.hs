{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IGMIssue where

import qualified Domain.Types.IGMIssue
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IGMIssue as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMIssue.IGMIssue -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IGMIssue.IGMIssue] -> m ())
createMany = traverse_ create

findByinternalIssueId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IGMIssue.IGMIssue -> m (Maybe Domain.Types.IGMIssue.IGMIssue))
findByinternalIssueId (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IGMIssue.IGMIssue -> m (Maybe Domain.Types.IGMIssue.IGMIssue))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMIssue.IGMIssue -> m ())
updateByPrimaryKey (Domain.Types.IGMIssue.IGMIssue {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.customerEmail customerEmail,
      Se.Set Beam.customerName customerName,
      Se.Set Beam.customerPhone customerPhone,
      Se.Set Beam.issueRaisedByMerchantId (Kernel.Types.Id.getId issueRaisedByMerchantId),
      Se.Set Beam.issueStatus issueStatus,
      Se.Set Beam.issueType issueType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.resolutionAction resolutionAction,
      Se.Set Beam.respondentAction respondentAction,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IGMIssue Domain.Types.IGMIssue.IGMIssue where
  fromTType' (Beam.IGMIssueT {..}) = do
    pure $
      Just
        Domain.Types.IGMIssue.IGMIssue
          { bookingId = Kernel.Types.Id.Id bookingId,
            createdAt = createdAt,
            customerEmail = customerEmail,
            customerName = customerName,
            customerPhone = customerPhone,
            id = Kernel.Types.Id.Id id,
            issueRaisedByMerchantId = Kernel.Types.Id.Id issueRaisedByMerchantId,
            issueStatus = issueStatus,
            issueType = issueType,
            merchantId = Kernel.Types.Id.Id merchantId,
            resolutionAction = resolutionAction,
            respondentAction = respondentAction,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IGMIssue Domain.Types.IGMIssue.IGMIssue where
  toTType' (Domain.Types.IGMIssue.IGMIssue {..}) = do
    Beam.IGMIssueT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.createdAt = createdAt,
        Beam.customerEmail = customerEmail,
        Beam.customerName = customerName,
        Beam.customerPhone = customerPhone,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.issueRaisedByMerchantId = Kernel.Types.Id.getId issueRaisedByMerchantId,
        Beam.issueStatus = issueStatus,
        Beam.issueType = issueType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.resolutionAction = resolutionAction,
        Beam.respondentAction = respondentAction,
        Beam.updatedAt = updatedAt
      }
