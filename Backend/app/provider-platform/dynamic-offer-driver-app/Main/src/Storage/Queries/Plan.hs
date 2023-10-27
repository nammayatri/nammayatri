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

import Domain.Types.Merchant
import Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as BeamP

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Plan -> m ()
create = createWithKV

fetchAllPlan :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Plan]
fetchAllPlan = findAllWithKV [Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId ""]

findByIdAndPaymentMode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Plan -> PaymentMode -> m (Maybe Plan)
findByIdAndPaymentMode (Id planId) paymentMode = findOneWithKV [Se.And [Se.Is BeamP.id $ Se.Eq planId, Se.Is BeamP.paymentMode $ Se.Eq paymentMode]]

findByMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [Plan]
findByMerchantId (Id merchantId) = findAllWithKV [Se.Is BeamP.merchantId $ Se.Eq merchantId]

findByMerchantIdAndPaymentMode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> PaymentMode -> m [Plan]
findByMerchantIdAndPaymentMode (Id merchantId) paymentMode = findAllWithKV [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.paymentMode $ Se.Eq paymentMode]]

findByMerchantIdAndType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> PlanType -> m [Plan]
findByMerchantIdAndType (Id merchantId) planType = findAllWithKV [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.planType $ Se.Eq planType]]

instance FromTType' BeamP.Plan Plan where
  fromTType' BeamP.PlanT {..} = do
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
            planType = planType
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
        BeamP.planType = planType
      }
