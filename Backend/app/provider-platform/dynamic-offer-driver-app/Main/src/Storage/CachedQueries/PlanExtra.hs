{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.PlanExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Plan
import Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Plan as Queries

findByIdAndPaymentModeWithServiceName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Plan -> PaymentMode -> ServiceNames -> m (Maybe Plan)
findByIdAndPaymentModeWithServiceName (Id planId) paymentMode serviceName =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makePlanIdAndPaymentModeKey (Id planId) paymentMode serviceName) >>= \case
    Just a -> pure a
    Nothing -> cacheByIdAndPaymentMode (Id planId) paymentMode serviceName /=<< Queries.findByIdAndPaymentModeWithServiceName (Id planId) paymentMode serviceName

cacheByIdAndPaymentMode :: (CacheFlow m r) => Id Plan -> PaymentMode -> ServiceNames -> Maybe Plan -> m ()
cacheByIdAndPaymentMode (Id planId) paymentMode serviceName plan = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makePlanIdAndPaymentModeKey (Id planId) paymentMode serviceName) plan expTime

------------------- -----------------------
findByMerchantOpCityIdWithServiceName :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> ServiceNames -> m [Plan]
findByMerchantOpCityIdWithServiceName (Id merchantOperatingCityId) serviceName =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey (Id merchantOperatingCityId) serviceName) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantId (Id merchantOperatingCityId) serviceName /=<< Queries.findByMerchantOpCityIdWithServiceName (Id merchantOperatingCityId) serviceName

cacheByMerchantId :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> ServiceNames -> [Plan] -> m ()
cacheByMerchantId (Id merchantOperatingCityId) serviceName plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdKey (Id merchantOperatingCityId) serviceName) plans expTime

------------------- -----------------------
findByMerchantOpCityIdAndPaymentModeWithServiceName ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Bool ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndPaymentModeWithServiceName (Id merchantOperatingCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdAndPaymentModeKey (Id merchantOperatingCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantIdAndPaymentMode (Id merchantOperatingCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan /=<< Queries.findByMerchantOpCityIdAndPaymentModeWithServiceName (Id merchantOperatingCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan

------------------- -----------------------
findByMerchantOpCityIdAndServiceName ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  ServiceNames ->
  Maybe Bool ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdAndServiceNameKey merchantOperatingCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantIdAndServiceName merchantOperatingCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan /=<< Queries.findByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan

cacheByMerchantIdAndServiceName ::
  (CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  ServiceNames ->
  Maybe Bool ->
  Maybe Bool ->
  [Plan] ->
  m ()
cacheByMerchantIdAndServiceName merchantOperatingCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdAndServiceNameKey merchantOperatingCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan) plans expTime

cacheByMerchantIdAndPaymentMode ::
  (CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Bool ->
  Maybe Bool ->
  [Plan] ->
  m ()
cacheByMerchantIdAndPaymentMode (Id merchantOperatingCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdAndPaymentModeKey (Id merchantOperatingCityId) paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan) plans expTime

findByMerchantOpCityIdAndTypeWithServiceName :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> PlanType -> ServiceNames -> DVC.VehicleCategory -> Bool -> m [Plan]
findByMerchantOpCityIdAndTypeWithServiceName (Id merchantOperatingCityId) planType serviceName vehicleCategory isDeprecated =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdAndTypeKey (Id merchantOperatingCityId) planType serviceName vehicleCategory isDeprecated) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantIdAndType (Id merchantOperatingCityId) planType serviceName vehicleCategory isDeprecated /=<< Queries.findByMerchantOpCityIdAndTypeWithServiceName (Id merchantOperatingCityId) planType serviceName vehicleCategory isDeprecated

cacheByMerchantIdAndType :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> PlanType -> ServiceNames -> DVC.VehicleCategory -> Bool -> [Plan] -> m ()
cacheByMerchantIdAndType (Id merchantOperatingCityId) planType serviceName vehicleCategory isDeprecated plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdAndTypeKey (Id merchantOperatingCityId) planType serviceName vehicleCategory isDeprecated) plans expTime

findByMerchantOpCityIdAndTypeWithServiceNameAndVariant ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Vehicle.VehicleVariant ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndTypeWithServiceNameAndVariant (Id merchantOperatingCityId) paymentMode serviceName mbVehicleVariant mbIsDeprecated =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdAndPaymentModeAndVariantKey (Id merchantOperatingCityId) paymentMode serviceName mbVehicleVariant mbIsDeprecated) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantIdAndTypeAndVariant (Id merchantOperatingCityId) paymentMode serviceName mbVehicleVariant mbIsDeprecated /=<< Queries.findByMerchantOpCityIdAndTypeWithServiceNameAndVariant (Id merchantOperatingCityId) paymentMode serviceName mbVehicleVariant

findByCityServiceAndVehicle ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Id DMOC.MerchantOperatingCity -> ServiceNames -> VehicleCategory -> Bool -> m [Plan])
findByCityServiceAndVehicle merchantOpCityId serviceName vehicleCategory isDeprecated = do
  Hedis.safeGet (makeCityServiceAndVehicleKey merchantOpCityId serviceName vehicleCategory isDeprecated)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp (makeCityServiceAndVehicleKey merchantOpCityId serviceName vehicleCategory isDeprecated) dataToBeCached expTime
              )
                /=<< Queries.findByCityServiceAndVehicle merchantOpCityId serviceName vehicleCategory isDeprecated
        )

cacheByMerchantIdAndTypeAndVariant ::
  (CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Vehicle.VehicleVariant ->
  Maybe Bool ->
  [Plan] ->
  m ()
cacheByMerchantIdAndTypeAndVariant (Id merchantOperatingCityId) paymentMode serviceName mbVehicleVariant mbIsDeprecated plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdAndPaymentModeAndVariantKey (Id merchantOperatingCityId) paymentMode serviceName mbVehicleVariant mbIsDeprecated) plans expTime

------------------- -----------------------
fetchAllPlan :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => m [Plan]
fetchAllPlan =
  Hedis.withCrossAppRedis (Hedis.safeGet makeAllPlanKey) >>= \case
    Just a -> pure a
    Nothing -> cacheAllPlan /=<< Queries.fetchAllPlan

cacheAllPlan :: (CacheFlow m r) => [Plan] -> m ()
cacheAllPlan plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp makeAllPlanKey plans expTime

makeAllPlanKey :: Text
makeAllPlanKey = "driver-offer:CachedQueries:Plan:PlanId-ALL"

makePlanIdAndPaymentModeKey :: Id Plan -> PaymentMode -> ServiceNames -> Text
makePlanIdAndPaymentModeKey id paymentMode serviceName = "driver-offer:CachedQueries:Plan:PlanId-" <> id.getId <> ":PaymentMode-" <> show paymentMode <> ":ServiceName-" <> show serviceName

makeMerchantIdAndPaymentModeKey :: Id DMOC.MerchantOperatingCity -> PaymentMode -> ServiceNames -> Maybe Bool -> Maybe Bool -> Text
makeMerchantIdAndPaymentModeKey merchantOpCityId paymentMode serviceName mbIsDeprecated mbIsFleetOwnerPlan =
  "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-"
    <> merchantOpCityId.getId
    <> ":PaymentMode-"
    <> show paymentMode
    <> ":ServiceName-"
    <> show serviceName
    <> ":IsDeprecated-"
    <> show mbIsDeprecated
    <> ":IsFleetOwnerPlan-"
    <> show mbIsFleetOwnerPlan

makeMerchantIdAndServiceNameKey :: Id DMOC.MerchantOperatingCity -> ServiceNames -> Maybe Bool -> Maybe Bool -> Text
makeMerchantIdAndServiceNameKey merchantOpCityId serviceName mbIsDeprecated mbIsFleetOwnerPlan =
  "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-"
    <> merchantOpCityId.getId
    <> ":ServiceName-"
    <> show serviceName
    <> ":IsDeprecated-"
    <> show mbIsDeprecated
    <> ":IsFleetOwnerPlan-"
    <> show mbIsFleetOwnerPlan

makeMerchantIdAndTypeKey :: Id DMOC.MerchantOperatingCity -> PlanType -> ServiceNames -> DVC.VehicleCategory -> Bool -> Text
makeMerchantIdAndTypeKey merchantOpCityId planType serviceName vehicleCategory isDeprecated = "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-" <> merchantOpCityId.getId <> ":PlanType-" <> show planType <> ":ServiceName-" <> show serviceName <> ":IsDeprecated-" <> show isDeprecated <> ":VehicleCategory-" <> show vehicleCategory

makeMerchantIdKey :: Id DMOC.MerchantOperatingCity -> ServiceNames -> Text
makeMerchantIdKey merchantOpCityId serviceName = "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-" <> merchantOpCityId.getId <> ":ServiceName-" <> show serviceName

makeMerchantIdAndPaymentModeAndVariantKey :: Id DMOC.MerchantOperatingCity -> PaymentMode -> ServiceNames -> Maybe Vehicle.VehicleVariant -> Maybe Bool -> Text
makeMerchantIdAndPaymentModeAndVariantKey merchantOpCityId paymentMode serviceName mbVehicleVariant mbIsDeprecated =
  "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-"
    <> merchantOpCityId.getId
    <> ":PaymentMode-"
    <> show paymentMode
    <> ":ServiceName-"
    <> show serviceName
    <> ":VehicleVariant-"
    <> show mbVehicleVariant
    <> ":IsDeprecated-"
    <> show mbIsDeprecated

makeIdKey :: Id DMOC.MerchantOperatingCity -> PaymentMode -> ServiceNames -> DVC.VehicleCategory -> Bool -> Text
makeIdKey merchantOpCityId planMode serviceName vehicleCategory isDeprecated = "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-" <> merchantOpCityId.getId <> ":PlanMode-" <> show planMode <> ":ServiceName-" <> show serviceName <> ":IsDeprecated-" <> show isDeprecated <> ":VehicleCategory-" <> show vehicleCategory

makeCityServiceAndVehicleKey :: Id DMOC.MerchantOperatingCity -> ServiceNames -> DVC.VehicleCategory -> Bool -> Text
makeCityServiceAndVehicleKey merchantOpCityId serviceName vehicleCategory isDeprecated = "driver-offer:CachedQueries:Plan:MerchantOperatingCityId-" <> merchantOpCityId.getId <> ":ServiceName-" <> show serviceName <> ":IsDeprecated-" <> show isDeprecated <> ":VehicleCategory-" <> show vehicleCategory
