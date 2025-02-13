module Storage.Queries.VendorFeeExtra where

import Domain.Types.DriverFee
import Domain.Types.VendorFee
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import SharedLogic.Payment (roundToTwoDecimalPlaces)
import qualified Storage.Beam.VendorFee as Beam
import Storage.Queries.OrphanInstances.VendorFee ()

-- Extra code goes here --

findAllByDriverFeeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DriverFee -> m [VendorFee]
findAllByDriverFeeId driverFeeId = findAllWithKV [Se.Is Beam.driverFeeId $ Se.Eq driverFeeId.getId]

findByVendorAndDriverFeeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Id DriverFee -> m (Maybe VendorFee)
findByVendorAndDriverFeeId vendorId driverFeeId = do findOneWithKV [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]

updateVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => VendorFee -> m ()
updateVendorFee vendorFee = do
  oldVendorFee <- findByVendorAndDriverFeeId vendorFee.vendorId vendorFee.driverFeeId
  case oldVendorFee of
    Just fee -> do
      now <- getCurrentTime
      updateWithKV
        [ Se.Set Beam.amount (roundToTwoDecimalPlaces (HighPrecMoney $ fee.amount.getHighPrecMoney + vendorFee.amount.getHighPrecMoney)),
          Se.Set Beam.updatedAt now
        ]
        [ Se.And
            [ Se.Is Beam.driverFeeId $ Se.Eq vendorFee.driverFeeId.getId,
              Se.Is Beam.vendorId $ Se.Eq vendorFee.vendorId
            ]
        ]
    _ -> pure ()

updateManyVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [VendorFee] -> m ()
updateManyVendorFee = traverse_ updateVendorFee
