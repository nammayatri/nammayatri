{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverBlockReason (module Storage.Queries.DriverBlockReason, module ReExport) where

import qualified Domain.Types.DriverBlockReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverBlockReason as Beam
import Storage.Queries.DriverBlockReasonExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DriverBlockReason.DriverBlockReason -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverBlockReason.DriverBlockReason] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverBlockReason.DriverBlockReason -> m (Maybe Domain.Types.DriverBlockReason.DriverBlockReason))
findByPrimaryKey (Kernel.Types.Id.Id reasonCode) = do findOneWithKV [Se.And [Se.Is Beam.reasonCode $ Se.Eq reasonCode]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverBlockReason.DriverBlockReason -> m ())
updateByPrimaryKey (Domain.Types.DriverBlockReason.DriverBlockReason {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.blockReason blockReason,
      Se.Set Beam.blockTimeInHours blockTimeInHours,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.reasonCode $ Se.Eq (Kernel.Types.Id.getId reasonCode)]]
