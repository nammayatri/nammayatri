{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorFee (module Storage.Queries.VendorFee, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.DriverFee
import qualified Domain.Types.VendorFee
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VendorFee as Beam
import Storage.Queries.VendorFeeExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorFee.VendorFee -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VendorFee.VendorFee] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> Data.Text.Text -> m (Maybe Domain.Types.VendorFee.VendorFee))
findByPrimaryKey driverFeeId vendorId = do findOneWithKV [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorFee.VendorFee -> m ())
updateByPrimaryKey (Domain.Types.VendorFee.VendorFee {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.isVendorFeeProcessedAt isVendorFeeProcessedAt,
      Se.Set Beam.splitMethod splitMethod,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]
