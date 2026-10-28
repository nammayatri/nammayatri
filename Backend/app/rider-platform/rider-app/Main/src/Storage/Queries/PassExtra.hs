{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassExtra where

import Data.Aeson (Value)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassType as DPassType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Pass as Beam
import Storage.Queries.OrphanInstances.Pass ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPass.Pass ->
  m (Maybe DPass.Pass)
findById passId = findOneWithKV [Se.Is Beam.id $ Se.Eq (getId passId)]

findAllByPassTypeIdAndEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassType.PassType ->
  Bool ->
  m [DPass.Pass]
findAllByPassTypeIdAndEnabled passTypeId enabled = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
          Se.Is Beam.enable $ Se.Eq enabled
        ]
    ]
    (Se.Asc Beam.order)
    Nothing
    Nothing

-- | Code is the discriminator the PASS_PURCHASE_ELIGIBILITY ruleset falls back to
-- when a pass type has no passEnum, so duplicates within a city make rules
-- ambiguous. Used by the dashboard create endpoint to reject collisions.
findByCodeAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe DPass.Pass)
findByCodeAndMerchantOperatingCityId code merchantOperatingCityId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.code $ Se.Eq code,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)
        ]
    ]

updateCatalogFields ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DPass.Pass ->
  m ()
updateCatalogFields DPass.Pass {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.applicableVehicleServiceTiers applicableVehicleServiceTiers,
      Se.Set Beam.autoApply autoApply,
      Se.Set Beam.benefit benefit,
      Se.Set Beam.benefitDescription benefitDescription,
      Se.Set Beam.code code,
      Se.Set Beam.dayCountRecommendations dayCountRecommendations,
      Se.Set Beam.description description,
      Se.Set Beam.documentsRequired documentsRequired,
      Se.Set Beam.dynamicDayCount dynamicDayCount,
      Se.Set Beam.enable enable,
      Se.Set Beam.formVerificationConfig formVerificationConfig,
      Se.Set Beam.frfsCancelLimit frfsCancelLimit,
      Se.Set Beam.frfsPriceOverrideApplicable frfsPriceOverrideApplicable,
      Se.Set Beam.maxFare maxFare,
      Se.Set Beam.maxValidDays maxValidDays,
      Se.Set Beam.maxValidTrips maxValidTrips,
      Se.Set Beam.merchantId (getId merchantId),
      Se.Set Beam.merchantOperatingCityId (getId merchantOperatingCityId),
      Se.Set Beam.minDaysToSuggestRenewal minDaysToSuggestRenewal,
      Se.Set Beam.minFare minFare,
      Se.Set Beam.minTripsAllowingOverlap minTripsAllowingOverlap,
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.maxSwitchCount ((.maxSwitchCount) <$> passConfig),
      Se.Set Beam.passTypeId (getId passTypeId),
      Se.Set Beam.pricingTiers pricingTiers,
      Se.Set Beam.purchaseEligibilityJsonLogic purchaseEligibilityJsonLogic,
      Se.Set Beam.redeemEligibilityJsonLogic redeemEligibilityJsonLogic,
      Se.Set Beam.timeOverlappingFrfsBookingsLimit timeOverlappingFrfsBookingsLimit,
      Se.Set Beam.vehicleType (Just vehicleType),
      Se.Set Beam.verificationValidity (Just verificationValidity),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId id)]

updateEnable ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Id DPass.Pass ->
  m ()
updateEnable enable passId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enable enable,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId passId)]

updateOverrideBenefitConfigJson ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Value ->
  Id DPass.Pass ->
  m ()
updateOverrideBenefitConfigJson overrideBenefitConfigJson passId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.overrideBenefitConfigJson overrideBenefitConfigJson,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId passId)]
