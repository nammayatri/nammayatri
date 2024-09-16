{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceUsageConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceUsageConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig] -> m ())
createMany = traverse_ create

findByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig))
findByMerchantOpCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateMerchantServiceUsageConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig -> m ())
updateMerchantServiceUsageConfig (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig {..}) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.getDistances getDistances,
      Se.Set Beam.getEstimatedPickupDistances getEstimatedPickupDistances,
      Se.Set Beam.getRoutes getRoutes,
      Se.Set Beam.snapToRoad snapToRoad,
      Se.Set Beam.getPlaceName getPlaceName,
      Se.Set Beam.getPlaceDetails getPlaceDetails,
      Se.Set Beam.autoComplete autoComplete,
      Se.Set Beam.smsProvidersPriorityList smsProvidersPriorityList,
      Se.Set Beam.snapToRoadProvidersList snapToRoadProvidersList,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

instance FromTType' Beam.MerchantServiceUsageConfig Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig where
  fromTType' (Beam.MerchantServiceUsageConfigT {..}) = do
    pure $
      Just
        Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig
          { aadhaarVerificationService = aadhaarVerificationService,
            autoComplete = autoComplete,
            autoCompletePriorityList = fromMaybe [] autoCompletePriorityList,
            backgroundVerification = backgroundVerification,
            createBankAccount = createBankAccount,
            createdAt = createdAt,
            driverBackgroundVerificationService = driverBackgroundVerificationService,
            faceVerificationService = faceVerificationService,
            getBankAccount = getBankAccount,
            getDistances = getDistances,
            getDistancesForCancelRide = getDistancesForCancelRide,
            getDistancesForCancelRidePriorityList = fromMaybe [] getDistancesForCancelRidePriorityList,
            getDistancesForScheduledRides = getDistancesForScheduledRides,
            getDistancesForScheduledRidesPriorityList = fromMaybe [] getDistancesForScheduledRidesPriorityList,
            getDistancesPriorityList = fromMaybe [] getDistancesPriorityList,
            getEstimatedPickupDistances = getEstimatedPickupDistances,
            getEstimatedPickupDistancesPriorityList = fromMaybe [] getEstimatedPickupDistancesPriorityList,
            getExophone = getExophone,
            getPickupRoutes = getPickupRoutes,
            getPickupRoutesPriorityList = fromMaybe [] getPickupRoutesPriorityList,
            getPlaceDetails = getPlaceDetails,
            getPlaceDetailsPriorityList = fromMaybe [] getPlaceDetailsPriorityList,
            getPlaceName = getPlaceName,
            getPlaceNamePriorityList = fromMaybe [] getPlaceNamePriorityList,
            getRoutes = getRoutes,
            getRoutesPriorityList = fromMaybe [] getRoutesPriorityList,
            getTripRoutes = getTripRoutes,
            getTripRoutesPriorityList = fromMaybe [] getTripRoutesPriorityList,
            initiateCall = initiateCall,
            issueTicketService = issueTicketService,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rectifyDistantPointsFailure = rectifyDistantPointsFailure,
            retryBankAccountLink = retryBankAccountLink,
            sdkVerificationService = sdkVerificationService,
            sendSearchRequestToDriver = sendSearchRequestToDriver,
            smsProvidersPriorityList = smsProvidersPriorityList,
            snapToRoad = snapToRoad,
            snapToRoadProvidersList = snapToRoadProvidersList,
            updatedAt = updatedAt,
            verificationProvidersPriorityList = verificationProvidersPriorityList,
            verificationService = verificationService,
            whatsappProvidersPriorityList = whatsappProvidersPriorityList
          }

instance ToTType' Beam.MerchantServiceUsageConfig Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig where
  toTType' (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig {..}) = do
    Beam.MerchantServiceUsageConfigT
      { Beam.aadhaarVerificationService = aadhaarVerificationService,
        Beam.autoComplete = autoComplete,
        Beam.autoCompletePriorityList = Just autoCompletePriorityList,
        Beam.backgroundVerification = backgroundVerification,
        Beam.createBankAccount = createBankAccount,
        Beam.createdAt = createdAt,
        Beam.driverBackgroundVerificationService = driverBackgroundVerificationService,
        Beam.faceVerificationService = faceVerificationService,
        Beam.getBankAccount = getBankAccount,
        Beam.getDistances = getDistances,
        Beam.getDistancesForCancelRide = getDistancesForCancelRide,
        Beam.getDistancesForCancelRidePriorityList = Just getDistancesForCancelRidePriorityList,
        Beam.getDistancesForScheduledRides = getDistancesForScheduledRides,
        Beam.getDistancesForScheduledRidesPriorityList = Just getDistancesForScheduledRidesPriorityList,
        Beam.getDistancesPriorityList = Just getDistancesPriorityList,
        Beam.getEstimatedPickupDistances = getEstimatedPickupDistances,
        Beam.getEstimatedPickupDistancesPriorityList = Just getEstimatedPickupDistancesPriorityList,
        Beam.getExophone = getExophone,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getPickupRoutesPriorityList = Just getPickupRoutesPriorityList,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.getPlaceDetailsPriorityList = Just getPlaceDetailsPriorityList,
        Beam.getPlaceName = getPlaceName,
        Beam.getPlaceNamePriorityList = Just getPlaceNamePriorityList,
        Beam.getRoutes = getRoutes,
        Beam.getRoutesPriorityList = Just getRoutesPriorityList,
        Beam.getTripRoutes = getTripRoutes,
        Beam.getTripRoutesPriorityList = Just getTripRoutesPriorityList,
        Beam.initiateCall = initiateCall,
        Beam.issueTicketService = issueTicketService,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rectifyDistantPointsFailure = rectifyDistantPointsFailure,
        Beam.retryBankAccountLink = retryBankAccountLink,
        Beam.sdkVerificationService = sdkVerificationService,
        Beam.sendSearchRequestToDriver = sendSearchRequestToDriver,
        Beam.smsProvidersPriorityList = smsProvidersPriorityList,
        Beam.snapToRoad = snapToRoad,
        Beam.snapToRoadProvidersList = snapToRoadProvidersList,
        Beam.updatedAt = updatedAt,
        Beam.verificationProvidersPriorityList = verificationProvidersPriorityList,
        Beam.verificationService = verificationService,
        Beam.whatsappProvidersPriorityList = whatsappProvidersPriorityList
      }
