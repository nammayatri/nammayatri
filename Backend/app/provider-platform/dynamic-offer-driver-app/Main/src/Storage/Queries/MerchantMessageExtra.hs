{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantMessageExtra where

import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantMessage as Beam
import Storage.Queries.MerchantMessage ()

-- Query that filters by both vehicleCategory and language
findByMerchantOpCityIdAndMessageKeyVehicleCategoryAndLanguage ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Domain.Types.MerchantMessage.MessageKey ->
  Maybe Domain.Types.VehicleCategory.VehicleCategory ->
  Maybe Kernel.External.Types.Language ->
  m (Maybe Domain.Types.MerchantMessage.MerchantMessage)
findByMerchantOpCityIdAndMessageKeyVehicleCategoryAndLanguage merchantOperatingCityId messageKey vehicleCategory language =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.language $ Se.Eq language
        ]
    ]
