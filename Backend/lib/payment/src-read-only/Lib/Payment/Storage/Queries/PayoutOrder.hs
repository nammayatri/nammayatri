{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutOrder where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutOrder as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder] -> m ())
createMany = traverse_ create

findAllByEntityNameAndEntityIds ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName -> [Kernel.Prelude.Maybe Kernel.Prelude.Text] -> m [Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder])
findAllByEntityNameAndEntityIds limit offset entityName entityId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.entityName $ Se.Eq entityName,
          Se.Is Beam.entityId $ Se.In entityId
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> m (Maybe Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByOrderId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder))
findByOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq orderId]

updatePayoutOrderStatus :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus -> Kernel.Prelude.Text -> m ())
updatePayoutOrderStatus status orderId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.orderId $ Se.Eq orderId]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder))
findByPrimaryKey id orderId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.orderId $ Se.Eq orderId]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountDetailsType accountDetailsType,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.price ((.amount) amount),
      Se.Set Beam.city city,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.customerEmailEncrypted (customerEmail & unEncrypted . encrypted),
      Se.Set Beam.customerEmailHash (customerEmail & hash),
      Se.Set Beam.customerId customerId,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityName entityName,
      Se.Set Beam.lastStatusCheckedAt lastStatusCheckedAt,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.mobileNoEncrypted (mobileNo & unEncrypted . encrypted),
      Se.Set Beam.mobileNoHash (mobileNo & hash),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vpa vpa
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.orderId $ Se.Eq orderId]]

instance FromTType' Beam.PayoutOrder Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder where
  fromTType' (Beam.PayoutOrderT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder
          { accountDetailsType = accountDetailsType,
            amount = Kernel.Types.Common.mkPrice currency price,
            city = city,
            createdAt = createdAt,
            customerEmail = EncryptedHashed (Encrypted customerEmailEncrypted) customerEmailHash,
            customerId = customerId,
            entityId = entityId,
            entityName = entityName,
            id = Kernel.Types.Id.Id id,
            lastStatusCheckedAt = lastStatusCheckedAt,
            merchantId = merchantId,
            mobileNo = EncryptedHashed (Encrypted mobileNoEncrypted) mobileNoHash,
            orderId = orderId,
            status = status,
            updatedAt = updatedAt,
            vpa = vpa
          }

instance ToTType' Beam.PayoutOrder Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder where
  toTType' (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder {..}) = do
    Beam.PayoutOrderT
      { Beam.accountDetailsType = accountDetailsType,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.price = (.amount) amount,
        Beam.city = city,
        Beam.createdAt = createdAt,
        Beam.customerEmailEncrypted = customerEmail & unEncrypted . encrypted,
        Beam.customerEmailHash = customerEmail & hash,
        Beam.customerId = customerId,
        Beam.entityId = entityId,
        Beam.entityName = entityName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastStatusCheckedAt = lastStatusCheckedAt,
        Beam.merchantId = merchantId,
        Beam.mobileNoEncrypted = mobileNo & unEncrypted . encrypted,
        Beam.mobileNoHash = mobileNo & hash,
        Beam.orderId = orderId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vpa = vpa
      }
