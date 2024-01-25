{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Plan
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as BeamP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Plan -> m ()
create = createWithKV

fetchAllPlan :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Plan]
fetchAllPlan = findAllWithKV [Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId ""]

findByIdAndPaymentModeWithServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Plan -> PaymentMode -> ServiceNames -> m (Maybe Plan)
findByIdAndPaymentModeWithServiceName (Id planId) paymentMode serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamP.id $ Se.Eq planId,
          Se.Is BeamP.paymentMode $ Se.Eq paymentMode,
          Se.Is BeamP.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findByMerchantOpCityIdWithServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> ServiceNames -> m [Plan]
findByMerchantOpCityIdWithServiceName (Id merchantOpCityId) serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamP.merchantOpCityId $ Se.Eq (Just merchantOpCityId),
          Se.Is BeamP.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findByMerchantOpCityIdAndPaymentModeWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndPaymentModeWithServiceName (Id merchantOpCityId) paymentMode serviceName mbIsDeprecated = do
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamP.merchantOpCityId $ Se.Eq (Just merchantOpCityId),
            Se.Is BeamP.paymentMode $ Se.Eq paymentMode,
            Se.Is BeamP.serviceName $ Se.Eq (Just serviceName)
          ]
            <> [Se.Is BeamP.isDeprecated $ Se.Eq mbIsDeprecated | isJust mbIsDeprecated]
        )
    ]

findByMerchantOpCityIdAndTypeWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PlanType ->
  ServiceNames ->
  m [Plan]
findByMerchantOpCityIdAndTypeWithServiceName (Id merchantOpCityId) planType serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamP.merchantOpCityId $ Se.Eq (Just merchantOpCityId),
          Se.Is BeamP.planType $ Se.Eq planType,
          Se.Is BeamP.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

instance FromTType' BeamP.Plan Plan where
  fromTType' BeamP.PlanT {..} = do
    merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
    merchantOperatingCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOpCityId) merchant Nothing
    pure $
      Just
        Plan
          { id = Id id,
            paymentMode = paymentMode,
            merchantId = Id merchantId,
            name = name,
            description = description,
            maxAmount = maxAmount,
            registrationAmount = registrationAmount,
            isOfferApplicable = isOfferApplicable,
            maxCreditLimit = maxCreditLimit,
            planBaseAmount = planBaseAmount,
            freeRideCount = freeRideCount,
            frequency = frequency,
            cgstPercentage = cgstPercentage,
            sgstPercentage = sgstPercentage,
            planType = planType,
            maxMandateAmount = maxMandateAmount,
            merchantOpCityId = merchantOperatingCityId,
            serviceName = fromMaybe YATRI_SUBSCRIPTION serviceName,
            eligibleForCoinDiscount = fromMaybe False eligibleForCoinDiscount,
            subscribedFlagToggleAllowed = fromMaybe False subscribedFlagToggleAllowed,
            isDeprecated = fromMaybe True isDeprecated
          }

instance ToTType' BeamP.Plan Plan where
  toTType' Plan {..} = do
    BeamP.PlanT
      { BeamP.id = getId id,
        BeamP.paymentMode = paymentMode,
        BeamP.merchantId = getId merchantId,
        BeamP.name = name,
        BeamP.description = description,
        BeamP.maxAmount = maxAmount,
        BeamP.registrationAmount = registrationAmount,
        BeamP.isOfferApplicable = isOfferApplicable,
        BeamP.maxCreditLimit = maxCreditLimit,
        BeamP.planBaseAmount = planBaseAmount,
        BeamP.freeRideCount = freeRideCount,
        BeamP.frequency = frequency,
        BeamP.cgstPercentage = cgstPercentage,
        BeamP.sgstPercentage = sgstPercentage,
        BeamP.planType = planType,
        BeamP.maxMandateAmount = maxMandateAmount,
        BeamP.merchantOpCityId = Just merchantOpCityId.getId,
        BeamP.serviceName = Just serviceName,
        BeamP.eligibleForCoinDiscount = Just eligibleForCoinDiscount,
        BeamP.subscribedFlagToggleAllowed = Just subscribedFlagToggleAllowed,
        BeamP.isDeprecated = Just isDeprecated
      }
