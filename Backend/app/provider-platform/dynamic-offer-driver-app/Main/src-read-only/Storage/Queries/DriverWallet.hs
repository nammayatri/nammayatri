{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverWallet (module Storage.Queries.DriverWallet, module ReExport) where

import qualified Domain.Types.DriverWallet
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Sequelize as Se
import qualified Storage.Beam.DriverWallet as Beam
import Storage.Queries.DriverWalletExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverWallet.DriverWallet -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverWallet.DriverWallet] -> m ())
createMany = traverse_ create

findAllByPayoutOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder) -> m [Domain.Types.DriverWallet.DriverWallet])
findAllByPayoutOrderId payoutOrderId = do findAllWithKV [Se.Is Beam.payoutOrderId $ Se.Eq (Kernel.Types.Id.getId <$> payoutOrderId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverWallet.DriverWallet -> m (Maybe Domain.Types.DriverWallet.DriverWallet))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverWallet.DriverWallet -> m ())
updateByPrimaryKey (Domain.Types.DriverWallet.DriverWallet {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.collectionAmount collectionAmount,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverPayable driverPayable,
      Se.Set Beam.gstDeduction gstDeduction,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.merchantPayable merchantPayable,
      Se.Set Beam.payoutOrderId (Kernel.Types.Id.getId <$> payoutOrderId),
      Se.Set Beam.payoutStatus payoutStatus,
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.runningBalance runningBalance,
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
