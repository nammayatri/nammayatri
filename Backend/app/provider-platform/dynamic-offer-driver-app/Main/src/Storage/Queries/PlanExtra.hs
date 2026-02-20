module Storage.Queries.PlanExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as BeamP
import Storage.Queries.OrphanInstances.Plan ()

-- Extra code goes here --

findByMerchantOpCityIdAndPaymentModeWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Bool ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndPaymentModeWithServiceName (Id merchantOpCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan = do
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamP.merchantOpCityId $ Se.Eq merchantOpCityId,
            Se.Is BeamP.paymentMode $ Se.Eq paymentMode,
            Se.Is BeamP.serviceName $ Se.Eq serviceName,
            Se.Is BeamP.isFleetOwnerPlan $ Se.Eq mbIsFleetOwnerPlan
          ]
            <> ( case mbIsDeprecated of
                   Nothing -> []
                   Just isDeprecated -> [Se.Is BeamP.isDeprecated $ Se.Eq isDeprecated]
               )
        )
    ]

findByMerchantOpCityIdAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  ServiceNames ->
  Maybe Bool ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndServiceName (Id merchantOpCityId) serviceName mbIsDeprecated mbIsFleetOwnerPlan = do
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamP.merchantOpCityId $ Se.Eq merchantOpCityId,
            Se.Is BeamP.serviceName $ Se.Eq serviceName,
            Se.Is BeamP.isFleetOwnerPlan $ Se.Eq mbIsFleetOwnerPlan
          ]
            <> ( case mbIsDeprecated of
                   Nothing -> []
                   Just isDeprecated -> [Se.Is BeamP.isDeprecated $ Se.Eq isDeprecated]
               )
        )
    ]

fetchAllPlan :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Plan]
fetchAllPlan = findAllWithKV [Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId ""]

fetchAllPlanByMerchantOperatingCityMbServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe ServiceNames -> m [Plan]
fetchAllPlanByMerchantOperatingCityMbServiceName merchantOpCityId mbServiceName = do
  findAllWithKV
    ( [ Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId "",
        Se.Is BeamP.merchantOpCityId $ Se.Eq merchantOpCityId.getId
      ]
        <> foldMap (\serviceName -> [Se.Is BeamP.serviceName $ Se.Eq serviceName]) mbServiceName
    )

updateByPrimaryKeyP :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Plan.Plan -> m ())
updateByPrimaryKeyP (Domain.Types.Plan.Plan {..}) = do
  updateWithKV
    [ Se.Set BeamP.allowStrikeOff (Just allowStrikeOff),
      Se.Set BeamP.basedOnEntity basedOnEntity,
      Se.Set BeamP.cgstPercentage cgstPercentage,
      Se.Set BeamP.description description,
      Se.Set BeamP.eligibleForCoinDiscount eligibleForCoinDiscount,
      Se.Set BeamP.freeRideCount freeRideCount,
      Se.Set BeamP.frequency frequency,
      Se.Set BeamP.isDeprecated isDeprecated,
      Se.Set BeamP.isOfferApplicable isOfferApplicable,
      Se.Set BeamP.listingPriority listingPriority,
      Se.Set BeamP.maxAmount maxAmount,
      Se.Set BeamP.maxCreditLimit maxCreditLimit,
      Se.Set BeamP.maxMandateAmount maxMandateAmount,
      Se.Set BeamP.merchantId (getId merchantId),
      Se.Set BeamP.merchantOpCityId (getId merchantOpCityId),
      Se.Set BeamP.name name,
      Se.Set BeamP.originalRegistrationAmount originalRegistrationAmount,
      Se.Set BeamP.paymentMode paymentMode,
      Se.Set BeamP.planBaseAmount planBaseAmount,
      Se.Set BeamP.planType planType,
      Se.Set BeamP.productOwnershipAmount (Just productOwnershipAmount),
      Se.Set BeamP.registrationAmount registrationAmount,
      Se.Set BeamP.serviceName serviceName,
      Se.Set BeamP.sgstPercentage sgstPercentage,
      Se.Set BeamP.subscribedFlagToggleAllowed subscribedFlagToggleAllowed,
      Se.Set BeamP.vehicleCategory (Just vehicleCategory),
      Se.Set BeamP.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is BeamP.id $ Se.Eq (getId id), Se.Is BeamP.paymentMode $ Se.Eq paymentMode]]

markAsDeprecated :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Plan -> m ()
markAsDeprecated (Id planId) =
  updateWithKV [Se.Set BeamP.isDeprecated True] [Se.Is BeamP.id $ Se.Eq planId]

markAsActive :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Plan -> m ()
markAsActive (Id planId) =
  updateWithKV [Se.Set BeamP.isDeprecated False] [Se.Is BeamP.id $ Se.Eq planId]
