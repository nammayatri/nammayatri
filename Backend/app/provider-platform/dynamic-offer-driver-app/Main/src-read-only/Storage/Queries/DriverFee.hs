{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverFee (module Storage.Queries.DriverFee, module ReExport) where

import qualified Domain.Types.DriverFee
import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as Beam
import Storage.Queries.DriverFeeExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DriverFee.DriverFee -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverFee.DriverFee] -> m ())
createMany = traverse_ create

updateAmountPaidByCoins :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateAmountPaidByCoins amountPaidByCoin (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.amountPaidByCoin amountPaidByCoin, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateAutopayPaymentStageById ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe Domain.Types.DriverFee.AutopayPaymentStage -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateAutopayPaymentStageById autopayPaymentStage stageUpdatedAt (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.autopayPaymentStage autopayPaymentStage, Se.Set Beam.stageUpdatedAt stageUpdatedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateBillNumberById :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateBillNumberById billNumber (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.billNumber billNumber, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateFeeType :: KvDbFlow m r => (Domain.Types.DriverFee.FeeType -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateFeeType feeType (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.feeType feeType, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateFeeWithoutDiscount :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateFeeWithoutDiscount feeWithoutDiscount (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.feeWithoutDiscount feeWithoutDiscount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateNotificationRetryCountById :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateNotificationRetryCountById notificationRetryCount (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.notificationRetryCount notificationRetryCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateOfferAndPlanDetails ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan) -> Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateOfferAndPlanDetails offerId planOfferTitle planId planMode (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.offerId offerId,
      Se.Set Beam.planOfferTitle planOfferTitle,
      Se.Set Beam.planId (Kernel.Types.Id.getId <$> planId),
      Se.Set Beam.planMode planMode,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq id]

updateOfferId :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateOfferId offerId (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.offerId offerId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateRetryCount :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m ())
updateRetryCount schedulerTryCount (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.schedulerTryCount schedulerTryCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> m (Maybe Domain.Types.DriverFee.DriverFee))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverFee.DriverFee -> m ())
updateByPrimaryKey (Domain.Types.DriverFee.DriverFee {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amountPaidByCoin amountPaidByCoin,
      Se.Set Beam.autopayPaymentStage autopayPaymentStage,
      Se.Set Beam.badDebtDeclarationDate badDebtDeclarationDate,
      Se.Set Beam.badDebtRecoveryDate badDebtRecoveryDate,
      Se.Set Beam.billNumber billNumber,
      Se.Set Beam.collectedAt collectedAt,
      Se.Set Beam.collectedBy collectedBy,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.endTime endTime,
      Se.Set Beam.feeType feeType,
      Se.Set Beam.feeWithoutDiscount feeWithoutDiscount,
      Se.Set Beam.govtCharges (Kernel.Prelude.roundToIntegral govtCharges),
      Se.Set Beam.govtChargesAmount (Kernel.Prelude.Just govtCharges),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Just (Kernel.Types.Id.getId merchantOperatingCityId)),
      Se.Set Beam.notificationRetryCount notificationRetryCount,
      Se.Set Beam.numRides numRides,
      Se.Set Beam.offerId offerId,
      Se.Set Beam.overlaySent overlaySent,
      Se.Set Beam.payBy payBy,
      Se.Set Beam.planId (Kernel.Types.Id.getId <$> planId),
      Se.Set Beam.planMode planMode,
      Se.Set Beam.planOfferTitle planOfferTitle,
      Se.Set Beam.cgst ((.cgst) platformFee),
      Se.Set Beam.platformFee ((.fee) platformFee),
      Se.Set Beam.sgst ((.sgst) platformFee),
      Se.Set Beam.schedulerTryCount schedulerTryCount,
      Se.Set Beam.serviceName (Just serviceName),
      Se.Set Beam.specialZoneAmount specialZoneAmount,
      Se.Set Beam.specialZoneRideCount specialZoneRideCount,
      Se.Set Beam.stageUpdatedAt stageUpdatedAt,
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.totalEarnings (Kernel.Prelude.roundToIntegral totalEarnings),
      Se.Set Beam.totalEarningsAmount (Kernel.Prelude.Just totalEarnings),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleNumber vehicleNumber
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
