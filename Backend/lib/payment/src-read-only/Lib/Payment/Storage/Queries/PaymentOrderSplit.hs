{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PaymentOrderSplit where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentOrderSplit
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PaymentOrderSplit as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit -> m (Maybe Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPaymentOrder ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m [Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit])
findByPaymentOrder paymentOrderId = do findAllWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit -> m (Maybe Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.price ((.amount) amount),
      Se.Set Beam.mdrBorneBy mdrBorneBy,
      Se.Set Beam.merchantCommission ((.amount) merchantCommission),
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vendorId vendorId
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PaymentOrderSplit Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit where
  fromTType' (Beam.PaymentOrderSplitT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit
          { amount = Kernel.Types.Common.mkPrice currency price,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            mdrBorneBy = mdrBorneBy,
            merchantCommission = Kernel.Types.Common.mkPrice currency merchantCommission,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            transactionId = transactionId,
            updatedAt = updatedAt,
            vendorId = vendorId
          }

instance ToTType' Beam.PaymentOrderSplit Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit where
  toTType' (Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit {..}) = do
    Beam.PaymentOrderSplitT
      { Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.price = (.amount) amount,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mdrBorneBy = mdrBorneBy,
        Beam.merchantCommission = (.amount) merchantCommission,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt,
        Beam.vendorId = vendorId
      }
