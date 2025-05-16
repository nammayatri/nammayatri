{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSPayment where

import qualified Domain.Types.FRFSPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSPayment as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSPayment.FRFSPayment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSPayment.FRFSPayment] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSPayment.FRFSPayment -> m (Maybe Domain.Types.FRFSPayment.FRFSPayment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSPayment.FRFSPayment -> m ())
updateByPrimaryKey (Domain.Types.FRFSPayment.FRFSPayment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency currency,
      Se.Set Beam.frfsTicketBookingId (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.paymentReferenceNumber paymentReferenceNumber,
      Se.Set Beam.status status,
      Se.Set Beam.transactionId (Kernel.Types.Id.getId transactionId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSPayment Domain.Types.FRFSPayment.FRFSPayment where
  fromTType' (Beam.FRFSPaymentT {..}) = do
    pure $
      Just
        Domain.Types.FRFSPayment.FRFSPayment
          { amount = amount,
            createdAt = createdAt,
            currency = currency,
            frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            paymentReferenceNumber = paymentReferenceNumber,
            status = status,
            transactionId = Kernel.Types.Id.Id transactionId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FRFSPayment Domain.Types.FRFSPayment.FRFSPayment where
  toTType' (Domain.Types.FRFSPayment.FRFSPayment {..}) = do
    Beam.FRFSPaymentT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentReferenceNumber = paymentReferenceNumber,
        Beam.status = status,
        Beam.transactionId = Kernel.Types.Id.getId transactionId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
