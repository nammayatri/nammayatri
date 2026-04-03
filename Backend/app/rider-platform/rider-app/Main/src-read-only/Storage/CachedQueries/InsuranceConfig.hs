{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.CachedQueries.InsuranceConfig where
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Domain.Types.InsuranceConfig
import qualified Storage.Queries.InsuranceConfig as Queries
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Common
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis



getInsuranceConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                      (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Common.TripCategory -> Domain.Types.VehicleCategory.VehicleCategory -> m (Kernel.Prelude.Maybe Domain.Types.InsuranceConfig.InsuranceConfig))
getInsuranceConfig merchantId merchantOperatingCityId tripCategory vehicleCategory = do (Hedis.safeGet $ "CachedQueries:InsuranceConfig:" <> ":MerchantId-" <> Kernel.Types.Id.getId merchantId <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":TripCategory-" <> show tripCategory <> ":VehicleCategory-" <> show vehicleCategory) >>= (\case
                                                                                                                                                                                                                                                                                                                                                                                  Just a -> pure (Just a)
                                                                                                                                                                                                                                                                                                                                                                                  Nothing -> flip whenJust (\dataToBeCached -> do {expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime);
                                                                                                                                                                                                                                                                                                                                                                                                                                   Hedis.setExp ("CachedQueries:InsuranceConfig:" <> ":MerchantId-" <> Kernel.Types.Id.getId merchantId <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":TripCategory-" <> show tripCategory <> ":VehicleCategory-" <> show vehicleCategory) dataToBeCached expTime}) /=<< Queries.findByMerchantIdAndMerchantOperatingCityIdAndTripCategoryAndVehicleCategory   merchantId   merchantOperatingCityId   tripCategory   vehicleCategory)



