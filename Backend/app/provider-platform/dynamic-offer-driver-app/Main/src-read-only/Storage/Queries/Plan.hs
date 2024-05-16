{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Plan (module Storage.Queries.Plan, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as Beam
import Storage.Queries.PlanExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.Plan.Plan -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Plan.Plan] -> m ())
createMany = traverse_ create

findByIdAndPaymentModeWithServiceName ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.PaymentMode -> Domain.Types.Plan.ServiceNames -> m (Maybe Domain.Types.Plan.Plan))
findByIdAndPaymentModeWithServiceName (Kernel.Types.Id.Id id) paymentMode serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id,
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByMerchantOpCityIdAndTypeWithServiceName ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.PlanType -> Domain.Types.Plan.ServiceNames -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityIdAndTypeWithServiceName (Kernel.Types.Id.Id merchantOpCityId) planType serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq merchantOpCityId,
          Se.Is Beam.planType $ Se.Eq planType,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByMerchantOpCityIdWithServiceName ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.ServiceNames -> m [Domain.Types.Plan.Plan])
findByMerchantOpCityIdWithServiceName (Kernel.Types.Id.Id merchantOpCityId) serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOpCityId $ Se.Eq merchantOpCityId,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Plan.Plan -> m (Maybe Domain.Types.Plan.Plan))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Plan.Plan -> m ())
updateByPrimaryKey (Domain.Types.Plan.Plan {..}) = do
  updateWithKV
    [ Se.Set Beam.basedOnEntity basedOnEntity,
      Se.Set Beam.cgstPercentage cgstPercentage,
      Se.Set Beam.description description,
      Se.Set Beam.eligibleForCoinDiscount eligibleForCoinDiscount,
      Se.Set Beam.freeRideCount freeRideCount,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.isDeprecated isDeprecated,
      Se.Set Beam.isOfferApplicable isOfferApplicable,
      Se.Set Beam.maxAmount maxAmount,
      Se.Set Beam.maxCreditLimit maxCreditLimit,
      Se.Set Beam.maxMandateAmount maxMandateAmount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOpCityId (Kernel.Types.Id.getId merchantOpCityId),
      Se.Set Beam.name name,
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.planBaseAmount planBaseAmount,
      Se.Set Beam.planType planType,
      Se.Set Beam.registrationAmount registrationAmount,
      Se.Set Beam.serviceName serviceName,
      Se.Set Beam.sgstPercentage sgstPercentage,
      Se.Set Beam.subscribedFlagToggleAllowed subscribedFlagToggleAllowed,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
