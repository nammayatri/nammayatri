{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SuspectStatusChangeRequest where

import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusChangeRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectStatusChangeRequest as Beam

create :: KvDbFlow m r => (Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest] -> m ())
createMany = traverse_ create

findAllByMerchantId :: KvDbFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest]))
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]

findAllByReqStatus :: KvDbFlow m r => (Domain.Types.SuspectFlagRequest.AdminApproval -> m ([Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest]))
findAllByReqStatus reqStatus = do findAllWithKV [Se.Is Beam.reqStatus $ Se.Eq reqStatus]

findBySuspectId :: KvDbFlow m r => (Kernel.Prelude.Text -> m (Maybe Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest))
findBySuspectId suspectId = do findOneWithKV [Se.Is Beam.suspectId $ Se.Eq suspectId]

findBySuspectIdAndMerchantId ::
  KvDbFlow m r =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m (Maybe Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest))
findBySuspectIdAndMerchantId suspectId merchantId = do findOneWithKV [Se.And [Se.Is Beam.suspectId $ Se.Eq suspectId, Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]]

findByPrimaryKey ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest -> m (Maybe Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest -> m ())
updateByPrimaryKey (Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantShortId merchantShortId,
      Se.Set Beam.reasonToChange reasonToChange,
      Se.Set Beam.reqStatus reqStatus,
      Se.Set Beam.suspectId suspectId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SuspectStatusChangeRequest Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest where
  fromTType' (Beam.SuspectStatusChangeRequestT {..}) = do
    pure $
      Just
        Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantShortId = merchantShortId,
            reasonToChange = reasonToChange,
            reqStatus = reqStatus,
            suspectId = suspectId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.SuspectStatusChangeRequest Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest where
  toTType' (Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest {..}) = do
    Beam.SuspectStatusChangeRequestT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantShortId = merchantShortId,
        Beam.reasonToChange = reasonToChange,
        Beam.reqStatus = reqStatus,
        Beam.suspectId = suspectId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
