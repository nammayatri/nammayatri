{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassPayment (module Storage.Queries.PurchasedPassPayment, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.Person
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
import Storage.Queries.PurchasedPassPaymentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PurchasedPassPayment.PurchasedPassPayment] -> m ())
createMany = traverse_ create

findAllByPurchasedPassId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m ([Domain.Types.PurchasedPassPayment.PurchasedPassPayment]))
findAllByPurchasedPassId purchasedPassId = do findAllWithKV [Se.Is Beam.purchasedPassId $ Se.Eq (Kernel.Types.Id.getId purchasedPassId)]

findAllByPurchasedPassIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Domain.Types.PurchasedPass.StatusType -> Data.Time.Calendar.Day -> m ([Domain.Types.PurchasedPassPayment.PurchasedPassPayment]))
findAllByPurchasedPassIdAndStatus limit offset purchasedPassId status endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq (Kernel.Types.Id.getId purchasedPassId),
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.endDate $ Se.GreaterThanOrEq endDate
        ]
    ]
    (Se.Asc Beam.startDate)
    limit
    offset

findAllByPurchasedPassIdAndStatusStartDateGreaterThan ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Domain.Types.PurchasedPass.StatusType -> Data.Time.Calendar.Day -> m ([Domain.Types.PurchasedPassPayment.PurchasedPassPayment]))
findAllByPurchasedPassIdAndStatusStartDateGreaterThan limit offset purchasedPassId status startDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq (Kernel.Types.Id.getId purchasedPassId),
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.startDate $ Se.GreaterThanOrEq startDate
        ]
    ]
    (Se.Asc Beam.startDate)
    limit
    offset

findAllWithPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.PurchasedPassPayment.PurchasedPassPayment]))
findAllWithPersonId limit offset personId = do findAllWithOptionsKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)] (Se.Desc Beam.createdAt) limit offset

findOneByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Domain.Types.PurchasedPassPayment.PurchasedPassPayment))
findOneByPaymentOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId)]

updateStatusByOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.StatusType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m ())
updateStatusByOrderId status orderId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId)]

updateStatusByPurchasedPassIdAndStartEndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PurchasedPass.StatusType -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> m ())
updateStatusByPurchasedPassIdAndStartEndDate status purchasedPassId startDate endDate = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status status, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq (Kernel.Types.Id.getId purchasedPassId),
          Se.Is Beam.startDate $ Se.Eq startDate,
          Se.Is Beam.endDate $ Se.Eq endDate
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m (Maybe Domain.Types.PurchasedPassPayment.PurchasedPassPayment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m ())
updateByPrimaryKey (Domain.Types.PurchasedPassPayment.PurchasedPassPayment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.endDate endDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.orderId (Kernel.Types.Id.getId orderId),
      Se.Set Beam.passCode passCode,
      Se.Set Beam.passName passName,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.profilePicture profilePicture,
      Se.Set Beam.purchasedPassId (Kernel.Types.Id.getId purchasedPassId),
      Se.Set Beam.startDate startDate,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
