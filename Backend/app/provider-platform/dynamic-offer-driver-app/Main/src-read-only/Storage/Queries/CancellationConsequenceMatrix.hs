{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationConsequenceMatrix where

import qualified Domain.Types.CancellationConsequenceMatrix
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationConsequenceMatrix as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix])
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateActiveById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix -> m ())
updateActiveById active id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.active active, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix -> m (Maybe Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix -> m ())
updateByPrimaryKey (Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.active active,
      Se.Set Beam.area area,
      Se.Set Beam.blacklistDriverForRiderSeconds blacklistDriverForRiderSeconds,
      Se.Set Beam.cancelledBy cancelledBy,
      Se.Set Beam.collectionMode collectionMode,
      Se.Set Beam.countsTowardCustomerCancellationStats countsTowardCustomerCancellationStats,
      Se.Set Beam.countsTowardDriverCancellationRate countsTowardDriverCancellationRate,
      Se.Set Beam.customerCommissionAndTax customerCommissionAndTax,
      Se.Set Beam.customerDeduction customerDeduction,
      Se.Set Beam.customerNotificationKey customerNotificationKey,
      Se.Set Beam.driverDeduction driverDeduction,
      Se.Set Beam.driverNotificationKey driverNotificationKey,
      Se.Set Beam.exemptDashboardBookings exemptDashboardBookings,
      Se.Set Beam.faultRule faultRule,
      Se.Set Beam.faultVerdict faultVerdict,
      Se.Set Beam.maxWaiveOffsPerPeriod maxWaiveOffsPerPeriod,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentInstrument paymentInstrument,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.waiveOffAllowed waiveOffAllowed,
      Se.Set Beam.waiveOffPeriodDays waiveOffPeriodDays,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CancellationConsequenceMatrix Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix where
  fromTType' (Beam.CancellationConsequenceMatrixT {..}) = do
    pure $
      Just
        Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix
          { active = active,
            area = area,
            blacklistDriverForRiderSeconds = blacklistDriverForRiderSeconds,
            cancelledBy = cancelledBy,
            collectionMode = collectionMode,
            countsTowardCustomerCancellationStats = countsTowardCustomerCancellationStats,
            countsTowardDriverCancellationRate = countsTowardDriverCancellationRate,
            customerCommissionAndTax = customerCommissionAndTax,
            customerDeduction = customerDeduction,
            customerNotificationKey = customerNotificationKey,
            driverDeduction = driverDeduction,
            driverNotificationKey = driverNotificationKey,
            exemptDashboardBookings = exemptDashboardBookings,
            faultRule = faultRule,
            faultVerdict = faultVerdict,
            id = Kernel.Types.Id.Id id,
            maxWaiveOffsPerPeriod = maxWaiveOffsPerPeriod,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentInstrument = paymentInstrument,
            tripCategory = tripCategory,
            vehicleServiceTier = vehicleServiceTier,
            waiveOffAllowed = waiveOffAllowed,
            waiveOffPeriodDays = waiveOffPeriodDays,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CancellationConsequenceMatrix Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix where
  toTType' (Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix {..}) = do
    Beam.CancellationConsequenceMatrixT
      { Beam.active = active,
        Beam.area = area,
        Beam.blacklistDriverForRiderSeconds = blacklistDriverForRiderSeconds,
        Beam.cancelledBy = cancelledBy,
        Beam.collectionMode = collectionMode,
        Beam.countsTowardCustomerCancellationStats = countsTowardCustomerCancellationStats,
        Beam.countsTowardDriverCancellationRate = countsTowardDriverCancellationRate,
        Beam.customerCommissionAndTax = customerCommissionAndTax,
        Beam.customerDeduction = customerDeduction,
        Beam.customerNotificationKey = customerNotificationKey,
        Beam.driverDeduction = driverDeduction,
        Beam.driverNotificationKey = driverNotificationKey,
        Beam.exemptDashboardBookings = exemptDashboardBookings,
        Beam.faultRule = faultRule,
        Beam.faultVerdict = faultVerdict,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxWaiveOffsPerPeriod = maxWaiveOffsPerPeriod,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentInstrument = paymentInstrument,
        Beam.tripCategory = tripCategory,
        Beam.vehicleServiceTier = vehicleServiceTier,
        Beam.waiveOffAllowed = waiveOffAllowed,
        Beam.waiveOffPeriodDays = waiveOffPeriodDays,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
