{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IGMIssue where

import qualified Data.Text
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

findByinternalIssueId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.IGMIssue.IGMIssue))
findByinternalIssueId internalIssueId = do findOneWithKV [Se.Is Beam.internalIssueId $ Se.Eq internalIssueId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IGMIssue.IGMIssue -> m (Maybe Domain.Types.IGMIssue.IGMIssue))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMIssue.IGMIssue -> m ())
updateByPrimaryKey (Domain.Types.IGMIssue.IGMIssue {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.internalIssueId internalIssueId,
      Se.Set Beam.issueStatus issueStatus,
      Se.Set Beam.issueType issueType,
      Se.Set Beam.respondentAction respondentAction,
      Se.Set Beam.respondentEmail respondentEmail,
      Se.Set Beam.respondentName respondentName,
      Se.Set Beam.respondentPhone respondentPhone,
      Se.Set Beam.respondingMerchantId (Kernel.Types.Id.getId respondingMerchantId),
      Se.Set Beam.respondentEntityType respondentEntityType,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IGMIssue Domain.Types.IGMIssue.IGMIssue where
  fromTType' (Beam.IGMIssueT {..}) = do
    pure $
      Just
        Domain.Types.IGMIssue.IGMIssue
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            internalIssueId = internalIssueId,
            issueStatus = issueStatus,
            issueType = issueType,
            respondentAction = respondentAction,
            respondentEmail = respondentEmail,
            respondentName = respondentName,
            respondentPhone = respondentPhone,
            respondingMerchantId = Kernel.Types.Id.Id respondingMerchantId,
            respondentEntityType = respondentEntityType,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.IGMIssue Domain.Types.IGMIssue.IGMIssue where
  toTType' (Domain.Types.IGMIssue.IGMIssue {..}) = do
    Beam.IGMIssueT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.internalIssueId = internalIssueId,
        Beam.issueStatus = issueStatus,
        Beam.issueType = issueType,
        Beam.respondentAction = respondentAction,
        Beam.respondentEmail = respondentEmail,
        Beam.respondentName = respondentName,
        Beam.respondentPhone = respondentPhone,
        Beam.respondingMerchantId = Kernel.Types.Id.getId respondingMerchantId,
        Beam.respondentEntityType = respondentEntityType,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
