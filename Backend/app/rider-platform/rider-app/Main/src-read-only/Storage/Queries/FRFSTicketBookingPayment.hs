{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingPayment where

import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPayment as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment] -> m ()
createMany = traverse_ createWithKV

findAllByStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> m ([Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment])
findAllByStatus status = do
  findAllWithKV
    [ Se.Is Beam.status $ Se.Eq status
    ]

findAllTicketBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ([Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment])
findAllTicketBookingId (Kernel.Types.Id.Id frfsTicketBookingId) = do
  findAllWithKV
    [ Se.Is Beam.frfsTicketBookingId $ Se.Eq frfsTicketBookingId
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m (Maybe (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPaymentOrderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findByPaymentOrderId (Kernel.Types.Id.Id paymentOrderId) = do
  findOneWithKV
    [ Se.Is Beam.paymentOrderId $ Se.Eq paymentOrderId
    ]

updateStatusByTicketBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateStatusByTicketBookingId status (Kernel.Types.Id.Id frfsTicketBookingId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status $ status,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.frfsTicketBookingId $ Se.Eq frfsTicketBookingId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m (Maybe (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m ()
updateByPrimaryKey Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.frfsTicketBookingId $ (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.paymentOrderId $ (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.status $ status,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.FRFSTicketBookingPayment Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment where
  fromTType' Beam.FRFSTicketBookingPaymentT {..} = do
    pure $
      Just
        Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment
          { frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            status = status,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBookingPayment Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment where
  toTType' Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment {..} = do
    Beam.FRFSTicketBookingPaymentT
      { Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.status = status,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
