{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Plan (module Storage.Queries.Plan, module ReExport) where

import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as Beam
import Storage.Queries.PlanExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Plan.Plan -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Plan.Plan] -> m ())
createMany = traverse_ create

findByCityServiceAndVehicle ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Extra.Plan.ServiceNames -> Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Bool -> m [Domain.Types.Plan.Plan])
findByCityServiceAndVehicle merchantOpCityId serviceName vehicleCategory isDeprecated = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.vehicleCategory $ Se.Eq (Kernel.Prelude.Just vehicleCategory),
          Se.Is Beam.isDeprecated $ Se.Eq isDeprecated
        ]
    ]

findByIdAndPaymentModeWithServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.PaymentMode -> Domain.Types.Extra.Plan.ServiceNames -> m (Maybe Domain.Types.Plan.Plan))
findByIdAndPaymentModeWithServiceName id paymentMode serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByMerchantOpCityAndServiceAndFleetOwnerPlan ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Extra.Plan.ServiceNames -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityAndServiceAndFleetOwnerPlan merchantOpCityId serviceName isFleetOwnerPlan = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.isFleetOwnerPlan $ Se.Eq isFleetOwnerPlan
        ]
    ]

findByMerchantOpCityIdAndTypeWithServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.PlanType -> Domain.Types.Extra.Plan.ServiceNames -> Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Bool -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId planType serviceName vehicleCategory isDeprecated = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.planType $ Se.Eq planType,
          Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.vehicleCategory $ Se.Eq (Kernel.Prelude.Just vehicleCategory),
          Se.Is Beam.isDeprecated $ Se.Eq isDeprecated
        ]
    ]

findByMerchantOpCityIdAndTypeWithServiceNameAndVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.PaymentMode -> Domain.Types.Extra.Plan.ServiceNames -> Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityIdAndTypeWithServiceNameAndVariant merchantOpCityId paymentMode serviceName vehicleVariant = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant
        ]
    ]

findByMerchantOpCityIdTypeServiceNameVehicle ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.PaymentMode -> Domain.Types.Extra.Plan.ServiceNames -> Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Bool -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityIdTypeServiceNameVehicle merchantOpCityId paymentMode serviceName vehicleCategory isDeprecated = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.vehicleCategory $ Se.Eq (Kernel.Prelude.Just vehicleCategory),
          Se.Is Beam.isDeprecated $ Se.Eq isDeprecated
        ]
    ]

findByMerchantOpCityIdWithServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Extra.Plan.ServiceNames -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityIdWithServiceName merchantOpCityId serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Plan.Plan -> m (Maybe Domain.Types.Plan.Plan))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Plan.Plan -> m ())
updateByPrimaryKey (Domain.Types.Plan.Plan {..}) = do
  updateWithKV
    [ Se.Set Beam.allowStrikeOff (Kernel.Prelude.Just allowStrikeOff),
      Se.Set Beam.basedOnEntity basedOnEntity,
      Se.Set Beam.billingType billingType,
      Se.Set Beam.cgstPercentage cgstPercentage,
      Se.Set Beam.description description,
      Se.Set Beam.eligibleForCoinDiscount eligibleForCoinDiscount,
      Se.Set Beam.freeRideCount freeRideCount,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.isDeprecated isDeprecated,
      Se.Set Beam.isFleetOwnerPlan isFleetOwnerPlan,
      Se.Set Beam.isOfferApplicable isOfferApplicable,
      Se.Set Beam.listingPriority listingPriority,
      Se.Set Beam.maxAmount maxAmount,
      Se.Set Beam.maxCreditLimit maxCreditLimit,
      Se.Set Beam.maxMandateAmount maxMandateAmount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOpCityId (Kernel.Types.Id.getId merchantOpCityId),
      Se.Set Beam.name name,
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.planBaseAmount planBaseAmount,
      Se.Set Beam.planType planType,
      Se.Set Beam.productOwnershipAmount (Kernel.Prelude.Just productOwnershipAmount),
      Se.Set Beam.registrationAmount registrationAmount,
      Se.Set Beam.serviceName serviceName,
      Se.Set Beam.sgstPercentage sgstPercentage,
      Se.Set Beam.subscribedFlagToggleAllowed subscribedFlagToggleAllowed,
      Se.Set Beam.validityInDays validityInDays,
      Se.Set Beam.vehicleCategory (Kernel.Prelude.Just vehicleCategory),
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
