{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ParkingTransaction where

import qualified Domain.Types.ParkingTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.ParkingTransaction as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ParkingTransaction.ParkingTransaction -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ParkingTransaction.ParkingTransaction] -> m ())
createMany = traverse_ create

findByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Domain.Types.ParkingTransaction.ParkingTransaction))
findByPaymentOrderId paymentOrderId = do findOneWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ParkingTransaction.StatusType -> Kernel.Types.Id.Id Domain.Types.ParkingTransaction.ParkingTransaction -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.ParkingTransaction.ParkingTransaction -> m (Maybe Domain.Types.ParkingTransaction.ParkingTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ParkingTransaction.ParkingTransaction -> m ())
updateByPrimaryKey (Domain.Types.ParkingTransaction.ParkingTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.endTime endTime,
      Se.Set Beam.parkingLotId parkingLotId,
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ParkingTransaction Domain.Types.ParkingTransaction.ParkingTransaction where
  fromTType' (Beam.ParkingTransactionT {..}) = do
    pure $
      Just
        Domain.Types.ParkingTransaction.ParkingTransaction
          { amount = amount,
            endTime = endTime,
            id = Kernel.Types.Id.Id id,
            parkingLotId = parkingLotId,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            startTime = startTime,
            status = status,
            vehicleNumber = vehicleNumber,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ParkingTransaction Domain.Types.ParkingTransaction.ParkingTransaction where
  toTType' (Domain.Types.ParkingTransaction.ParkingTransaction {..}) = do
    Beam.ParkingTransactionT
      { Beam.amount = amount,
        Beam.endTime = endTime,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.parkingLotId = parkingLotId,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.vehicleNumber = vehicleNumber,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
