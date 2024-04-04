{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SuspectFlagRequest (module Storage.Queries.SuspectFlagRequest, module ReExport) where

import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.SuspectFlagRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectFlagRequest as Beam
import Storage.Queries.SuspectFlagRequestExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> m ())
createMany = traverse_ create

findAllByDlAndAdminApprovalAndMerchantId ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest]))
findAllByDlAndAdminApprovalAndMerchantId dl adminApproval merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.dl $ Se.Eq dl,
          Se.Is Beam.adminApproval $ Se.Eq adminApproval,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)
        ]
    ]

findAllByMerchantIdAndDl ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest]))
findAllByMerchantIdAndDl merchantId dl = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.dl $ Se.Eq dl]]

findAllByMerchantIdAndVoterId ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest]))
findAllByMerchantIdAndVoterId merchantId voterId = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.voterId $ Se.Eq voterId]]

findByDlOrVoterId :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByDlOrVoterId dl voterId = do findOneWithKV [Se.Or [Se.Is Beam.dl $ Se.Eq dl, Se.Is Beam.voterId $ Se.Eq voterId]]

findByMerchantIdAndAdminApprovalAndDl ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Domain.Types.SuspectFlagRequest.AdminApproval -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndAdminApprovalAndDl merchantId adminApproval dl = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq adminApproval,
          Se.Is Beam.dl $ Se.Eq dl
        ]
    ]

findByMerchantIdAndAdminApprovalAndVoterId ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Domain.Types.SuspectFlagRequest.AdminApproval -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndAdminApprovalAndVoterId merchantId adminApproval voterId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq adminApproval,
          Se.Is Beam.voterId $ Se.Eq voterId
        ]
    ]

findByMerchantIdAndDl ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndDl merchantId dl = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.dl $ Se.Eq dl]]

findBydl :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findBydl dl = do findOneWithKV [Se.Is Beam.dl $ Se.Eq dl]

updateAdminApprovalById :: KvDbFlow m r => (Domain.Types.SuspectFlagRequest.AdminApproval -> Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> m ())
updateAdminApprovalById adminApproval (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.adminApproval adminApproval, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> m (Maybe Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> m ())
updateByPrimaryKey (Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.adminApproval adminApproval,
      Se.Set Beam.approvedBy approvedBy,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.dl dl,
      Se.Set Beam.firDetails firDetails,
      Se.Set Beam.firstName firstName,
      Se.Set Beam.flaggedBy flaggedBy,
      Se.Set Beam.flaggedCategory flaggedCategory,
      Se.Set Beam.flaggedReason flaggedReason,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.merchantShortId merchantShortId,
      Se.Set Beam.mobileNumber mobileNumber,
      Se.Set Beam.totalComplaintsCount totalComplaintsCount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.voterId voterId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
