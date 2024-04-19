{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IGMIssue where

import qualified Data.Text
import qualified Domain.Types.IGMIssue
import qualified Domain.Types.Person
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

findAllByRiderIdandStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.IGMIssue.Status -> m ([Domain.Types.IGMIssue.IGMIssue]))
findAllByRiderIdandStatus (Kernel.Types.Id.Id riderId) issueStatus = do findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq riderId, Se.Is Beam.issueStatus $ Se.Eq issueStatus]]

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMIssue.Status -> m ([Domain.Types.IGMIssue.IGMIssue]))
findAllByStatus issueStatus = do findAllWithKV [Se.Is Beam.issueStatus $ Se.Eq issueStatus]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.IGMIssue.IGMIssue))
findByTransactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IGMIssue.IGMIssue -> m (Maybe Domain.Types.IGMIssue.IGMIssue))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMIssue.IGMIssue -> m ())
updateByPrimaryKey (Domain.Types.IGMIssue.IGMIssue {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.issueStatus issueStatus,
      Se.Set Beam.issueType issueType,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.respondentAction respondentAction,
      Se.Set Beam.respondentEmail respondentEmail,
      Se.Set Beam.respondentEntityType respondentEntityType,
      Se.Set Beam.respondentName respondentName,
      Se.Set Beam.respondentPhone respondentPhone,
      Se.Set Beam.respondingMerchantId respondingMerchantId,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.transactionId transactionId,
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
            id = Kernel.Types.Id.Id id,
            issueStatus = issueStatus,
            issueType = issueType,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            respondentAction = respondentAction,
            respondentEmail = respondentEmail,
            respondentEntityType = respondentEntityType,
            respondentName = respondentName,
            respondentPhone = respondentPhone,
            respondingMerchantId = respondingMerchantId,
            riderId = Kernel.Types.Id.Id riderId,
            transactionId = transactionId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IGMIssue Domain.Types.IGMIssue.IGMIssue where
  toTType' (Domain.Types.IGMIssue.IGMIssue {..}) = do
    Beam.IGMIssueT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.issueStatus = issueStatus,
        Beam.issueType = issueType,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.respondentAction = respondentAction,
        Beam.respondentEmail = respondentEmail,
        Beam.respondentEntityType = respondentEntityType,
        Beam.respondentName = respondentName,
        Beam.respondentPhone = respondentPhone,
        Beam.respondingMerchantId = respondingMerchantId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt
      }
