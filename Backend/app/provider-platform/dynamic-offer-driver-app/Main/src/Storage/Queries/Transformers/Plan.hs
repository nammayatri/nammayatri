{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Plan where

import qualified Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.SubscriptionConfig as CQP
import qualified Storage.Queries.Person as QP

getCategoryFromSubscriptionConfig ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe DVC.VehicleCategory ->
  Text ->
  DPlan.ServiceNames ->
  m DVC.VehicleCategory
getCategoryFromSubscriptionConfig mbVehicleCategory merchantOpCityId serviceName = do
  case mbVehicleCategory of
    Just vehicleCategory -> pure vehicleCategory
    Nothing -> do
      subscriptionConfig <- CQP.findSubscriptionConfigsByMerchantOpCityIdAndServiceName (Id merchantOpCityId) serviceName
      return $ fromMaybe DVC.AUTO_CATEGORY (subscriptionConfig <&> (.defaultCityVehicleCategory))
