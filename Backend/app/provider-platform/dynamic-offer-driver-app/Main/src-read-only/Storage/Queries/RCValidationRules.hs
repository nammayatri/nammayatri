{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.RCValidationRules where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.RCValidationRules
import qualified Storage.Beam.RCValidationRules as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RCValidationRules.RCValidationRules -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RCValidationRules.RCValidationRules] -> m ())
createMany = traverse_ create
findByCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RCValidationRules.RCValidationRules))
findByCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RCValidationRules.RCValidationRules -> m (Maybe Domain.Types.RCValidationRules.RCValidationRules))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RCValidationRules.RCValidationRules -> m ())
updateByPrimaryKey (Domain.Types.RCValidationRules.RCValidationRules {..}) = do {_now <- getCurrentTime;
                                                                                 updateWithKV [Se.Set Beam.fuelType fuelType,
                                                                                               Se.Set Beam.maxVehicleAge maxVehicleAge,
                                                                                               Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                               Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                               Se.Set Beam.vehicleClass vehicleClass,
                                                                                               Se.Set Beam.vehicleOEM vehicleOEM,
                                                                                               Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.RCValidationRules Domain.Types.RCValidationRules.RCValidationRules
    where fromTType' (Beam.RCValidationRulesT {..}) = do pure $ Just Domain.Types.RCValidationRules.RCValidationRules{fuelType = fuelType,
                                                                                                                      id = Kernel.Types.Id.Id id,
                                                                                                                      maxVehicleAge = maxVehicleAge,
                                                                                                                      merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                      merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                      vehicleClass = vehicleClass,
                                                                                                                      vehicleOEM = vehicleOEM,
                                                                                                                      createdAt = createdAt,
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.RCValidationRules Domain.Types.RCValidationRules.RCValidationRules
    where toTType' (Domain.Types.RCValidationRules.RCValidationRules {..}) = do Beam.RCValidationRulesT{Beam.fuelType = fuelType,
                                                                                                        Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.maxVehicleAge = maxVehicleAge,
                                                                                                        Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                        Beam.vehicleClass = vehicleClass,
                                                                                                        Beam.vehicleOEM = vehicleOEM,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.updatedAt = updatedAt}



