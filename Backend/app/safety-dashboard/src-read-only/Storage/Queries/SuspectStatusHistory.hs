{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SuspectStatusHistory (module Storage.Queries.SuspectStatusHistory, module ReExport) where

import qualified Domain.Types.SuspectStatusHistory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectStatusHistory as Beam
import Storage.Queries.SuspectStatusHistoryExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.SuspectStatusHistory.SuspectStatusHistory -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.SuspectStatusHistory.SuspectStatusHistory -> m (Maybe Domain.Types.SuspectStatusHistory.SuspectStatusHistory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.SuspectStatusHistory.SuspectStatusHistory -> m ())
updateByPrimaryKey (Domain.Types.SuspectStatusHistory.SuspectStatusHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.adminApproval adminApproval,
      Se.Set Beam.approvedBy approvedBy,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.dl dl,
      Se.Set Beam.firstName firstName,
      Se.Set Beam.flaggedBy flaggedBy,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.merchantShortId merchantShortId,
      Se.Set Beam.statusChangedReason statusChangedReason,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.voterId voterId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
