{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DiscountExtra where

import qualified Domain.Types.Discount
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Discount as Beam
import Storage.Queries.OrphanInstances.Discount

-- Extra code goes here --

findByFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Bool ->
  Maybe Text ->
  Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan) ->
  Maybe Domain.Types.VehicleCategory.VehicleCategory ->
  Maybe Domain.Types.Plan.PaymentMode ->
  m [Domain.Types.Discount.Discount]
findByFilters merchantOpCityId enabled mbDiscountType mbPlanId mbVehicleCategory mbPaymentMode = do
  let baseConditions =
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOpCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
      discountTypeCondition = maybe [] (\dt -> [Se.Is Beam.discountType $ Se.Eq dt]) mbDiscountType
      planIdCondition = maybe [] (\pid -> [Se.Is Beam.planId $ Se.Eq (Just (Kernel.Types.Id.getId pid))]) mbPlanId
      vehicleCategoryCondition = maybe [] (\vc -> [Se.Is Beam.vehicleCategory $ Se.Eq (Just vc)]) mbVehicleCategory
      paymentModeCondition = maybe [] (\pm -> [Se.Is Beam.paymentMode $ Se.Eq (Just pm)]) mbPaymentMode
      allConditions = baseConditions <> discountTypeCondition <> planIdCondition <> vehicleCategoryCondition <> paymentModeCondition
  findAllWithKV [Se.And allConditions]
