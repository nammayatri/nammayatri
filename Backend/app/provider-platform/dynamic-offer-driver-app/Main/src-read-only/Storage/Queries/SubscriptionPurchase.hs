{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SubscriptionPurchase (module Storage.Queries.SubscriptionPurchase, module ReExport) where

import qualified Domain.Types.SubscriptionPurchase
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.SubscriptionPurchase as Beam
import Storage.Queries.SubscriptionPurchaseExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SubscriptionPurchase.SubscriptionPurchase -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SubscriptionPurchase.SubscriptionPurchase] -> m ())
createMany = traverse_ create

findActiveByOwner ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.SubscriptionPurchase.SubscriptionOwnerType -> Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> m (Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchase))
findActiveByOwner ownerId ownerType status = do findOneWithKV [Se.And [Se.Is Beam.ownerId $ Se.Eq ownerId, Se.Is Beam.ownerType $ Se.Eq ownerType, Se.Is Beam.status $ Se.Eq status]]

findAllByOwnerAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.SubscriptionPurchase.SubscriptionOwnerType -> Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> m [Domain.Types.SubscriptionPurchase.SubscriptionPurchase])
findAllByOwnerAndStatus ownerId ownerType status = do findAllWithKV [Se.And [Se.Is Beam.ownerId $ Se.Eq ownerId, Se.Is Beam.ownerType $ Se.Eq ownerType, Se.Is Beam.status $ Se.Eq status]]

findByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchase))
findByPaymentOrderId paymentOrderId = do findOneWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> Kernel.Types.Id.Id Domain.Types.SubscriptionPurchase.SubscriptionPurchase -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SubscriptionPurchase.SubscriptionPurchase -> m (Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchase))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SubscriptionPurchase.SubscriptionPurchase -> m ())
updateByPrimaryKey (Domain.Types.SubscriptionPurchase.SubscriptionPurchase {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enableServiceUsageCharge enableServiceUsageCharge,
      Se.Set Beam.expiryDate expiryDate,
      Se.Set Beam.financeInvoiceId (Kernel.Types.Id.getId <$> financeInvoiceId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.ownerId ownerId,
      Se.Set Beam.ownerType ownerType,
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.planFee planFee,
      Se.Set Beam.planFrequency planFrequency,
      Se.Set Beam.planId (Kernel.Types.Id.getId planId),
      Se.Set Beam.planRideCredit planRideCredit,
      Se.Set Beam.purchaseTimestamp purchaseTimestamp,
      Se.Set Beam.serviceName serviceName,
      Se.Set Beam.status status,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.waiveOfMode waiveOfMode,
      Se.Set Beam.waiveOffEnabledOn waiveOffEnabledOn,
      Se.Set Beam.waiveOffValidTill waiveOffValidTill,
      Se.Set Beam.waiverOffPercentage waiverOffPercentage,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
