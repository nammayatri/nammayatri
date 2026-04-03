{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.CachedQueries.Plan (module Storage.CachedQueries.Plan, module ReExport) where
import Kernel.Prelude
import Kernel.Utils.Common
import Storage.CachedQueries.PlanExtra as ReExport
import qualified Domain.Types.Plan
import qualified Storage.Queries.Plan as Queries
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis



deletePlansKeys :: CacheFlow m r =>
                   (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.PaymentMode -> Domain.Types.Extra.Plan.ServiceNames -> Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Bool -> m ())
deletePlansKeys merchantOpCityId paymentMode serviceName vehicleCategory isDeprecated = do (Hedis.del $ makeIdKey   merchantOpCityId   paymentMode   serviceName   vehicleCategory   isDeprecated)
findByMerchantOpCityIdTypeServiceNameVehicle :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Plan.PaymentMode -> Domain.Types.Extra.Plan.ServiceNames -> Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Bool -> m ([Domain.Types.Plan.Plan]))
findByMerchantOpCityIdTypeServiceNameVehicle merchantOpCityId paymentMode serviceName vehicleCategory isDeprecated = do (Hedis.safeGet $ makeIdKey   merchantOpCityId   paymentMode   serviceName   vehicleCategory   isDeprecated) >>= (\case
                                                                                                                                                                                                                                             Just a -> pure a
                                                                                                                                                                                                                                             Nothing -> (\dataToBeCached -> do {expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime);
                                                                                                                                                                                                                                                                                Hedis.setExp (makeIdKey   merchantOpCityId   paymentMode   serviceName   vehicleCategory   isDeprecated) dataToBeCached expTime}) /=<< Queries.findByMerchantOpCityIdTypeServiceNameVehicle   merchantOpCityId   paymentMode   serviceName   vehicleCategory   isDeprecated)



