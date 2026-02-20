module Storage.Queries.BecknConfigExtra where

import qualified BecknV2.OnDemand.Enums
import Control.Lens ((^?), _head)
import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BecknConfig as Beam
import Storage.Queries.OrphanInstances.BecknConfig ()

-- Extra code goes here --

findByMerchantIdDomainAndVehicle ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByMerchantIdDomainAndVehicle merchantId domain vehicleCategory = do (^? _head) <$> findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]

findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.BecknConfig.BecknConfig)))
findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCityId merchantId domain vehicleCategory = do
  configs <- findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId), Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]
  case configs ^? _head of
    Just config -> return (Just config)
    Nothing -> findByMerchantIdDomainAndVehicle merchantId domain vehicleCategory
