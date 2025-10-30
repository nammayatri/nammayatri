{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassPayment where

import qualified Domain.Types.PurchasedPass
import qualified Domain.Types.PurchasedPassPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPassPayment as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PurchasedPassPayment.PurchasedPassPayment] -> m ())
createMany = traverse_ create

findAllByPurchasedPassId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m [Domain.Types.PurchasedPassPayment.PurchasedPassPayment])
findAllByPurchasedPassId purchasedPassId = do findAllWithKV [Se.Is Beam.purchasedPassId $ Se.Eq (Kernel.Types.Id.getId purchasedPassId)]

findOneByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Domain.Types.PurchasedPassPayment.PurchasedPassPayment))
findOneByPaymentOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m (Maybe Domain.Types.PurchasedPassPayment.PurchasedPassPayment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m ())
updateByPrimaryKey (Domain.Types.PurchasedPassPayment.PurchasedPassPayment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.endDate endDate,
      Se.Set Beam.orderId (Kernel.Types.Id.getId orderId),
      Se.Set Beam.purchasedPassId (Kernel.Types.Id.getId purchasedPassId),
      Se.Set Beam.startDate startDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PurchasedPassPayment Domain.Types.PurchasedPassPayment.PurchasedPassPayment where
  fromTType' (Beam.PurchasedPassPaymentT {..}) = do
    pure $
      Just
        Domain.Types.PurchasedPassPayment.PurchasedPassPayment
          { endDate = endDate,
            id = Kernel.Types.Id.Id id,
            orderId = Kernel.Types.Id.Id orderId,
            purchasedPassId = Kernel.Types.Id.Id purchasedPassId,
            startDate = startDate,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PurchasedPassPayment Domain.Types.PurchasedPassPayment.PurchasedPassPayment where
  toTType' (Domain.Types.PurchasedPassPayment.PurchasedPassPayment {..}) = do
    Beam.PurchasedPassPaymentT
      { Beam.endDate = endDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.purchasedPassId = Kernel.Types.Id.getId purchasedPassId,
        Beam.startDate = startDate,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
