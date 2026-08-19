{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSBookingGroup where

import qualified Domain.Types.FRFSBookingGroup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSBookingGroup as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSBookingGroup.FRFSBookingGroup] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m (Maybe Domain.Types.FRFSBookingGroup.FRFSBookingGroup))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePaymentOrderShortIdById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Types.Id.Id Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m ())
updatePaymentOrderShortIdById paymentOrderShortId id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.paymentOrderShortId (Kernel.Types.Id.getShortId <$> paymentOrderShortId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingGroup.FRFSBookingGroupStatus -> Kernel.Types.Id.Id Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTotalPriceAndStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.Price -> Domain.Types.FRFSBookingGroup.FRFSBookingGroupStatus -> Kernel.Types.Id.Id Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m ())
updateTotalPriceAndStatusById totalPrice status id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) totalPrice),
      Se.Set Beam.price ((.amount) totalPrice),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m (Maybe Domain.Types.FRFSBookingGroup.FRFSBookingGroup))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingGroup.FRFSBookingGroup -> m ())
updateByPrimaryKey (Domain.Types.FRFSBookingGroup.FRFSBookingGroup {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentOrderShortId (Kernel.Types.Id.getShortId <$> paymentOrderShortId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.status status,
      Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) totalPrice),
      Se.Set Beam.price ((.amount) totalPrice),
      Se.Set Beam.totalSlots totalSlots,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSBookingGroup Domain.Types.FRFSBookingGroup.FRFSBookingGroup where
  fromTType' (Beam.FRFSBookingGroupT {..}) = do
    pure $
      Just
        Domain.Types.FRFSBookingGroup.FRFSBookingGroup
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentOrderShortId = Kernel.Types.Id.ShortId <$> paymentOrderShortId,
            riderId = Kernel.Types.Id.Id riderId,
            status = status,
            totalPrice = Kernel.Types.Common.mkPrice currency price,
            totalSlots = totalSlots,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSBookingGroup Domain.Types.FRFSBookingGroup.FRFSBookingGroup where
  toTType' (Domain.Types.FRFSBookingGroup.FRFSBookingGroup {..}) = do
    Beam.FRFSBookingGroupT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentOrderShortId = Kernel.Types.Id.getShortId <$> paymentOrderShortId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.status = status,
        Beam.currency = ((Kernel.Prelude.Just . (.currency))) totalPrice,
        Beam.price = (.amount) totalPrice,
        Beam.totalSlots = totalSlots,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
