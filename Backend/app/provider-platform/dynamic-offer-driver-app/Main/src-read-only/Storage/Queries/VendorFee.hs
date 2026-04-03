{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.VendorFee (module Storage.Queries.VendorFee, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.VendorFeeExtra as ReExport
import qualified Domain.Types.VendorFee
import qualified Storage.Beam.VendorFee as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.DriverFee
import qualified Data.Text
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorFee.VendorFee -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VendorFee.VendorFee] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> Data.Text.Text -> m (Maybe Domain.Types.VendorFee.VendorFee))
findByPrimaryKey driverFeeId vendorId = do findOneWithKV [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorFee.VendorFee -> m ())
updateByPrimaryKey (Domain.Types.VendorFee.VendorFee {..}) = do {_now <- getCurrentTime;
                                                                 updateWithKV [Se.Set Beam.amount amount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]}



