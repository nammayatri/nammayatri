{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceUsageConfig where

import qualified ChatCompletion.Types
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Prelude
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

updateSmsProvidersPriorityList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.External.SMS.Types.SmsService] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateSmsProvidersPriorityList smsProvidersPriorityList merchantOperatingCityId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.smsProvidersPriorityList smsProvidersPriorityList, Se.Set Beam.updatedAt _now] [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateSnapToRoadProvidersPriorityList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.External.Maps.Types.MapsService] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateSnapToRoadProvidersPriorityList snapToRoadProvidersList merchantOperatingCityId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.snapToRoadProvidersList snapToRoadProvidersList, Se.Set Beam.updatedAt _now] [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateWhatsappProvidersPriorityList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.External.Whatsapp.Types.WhatsappService] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateWhatsappProvidersPriorityList whatsappProvidersPriorityList merchantOperatingCityId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.whatsappProvidersPriorityList whatsappProvidersPriorityList,
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
            backgroundVerification = backgroundVerification,
            createBankAccount = createBankAccount,
            createdAt = createdAt,
            dashboardGstVerificationService = dashboardGstVerificationService,
            dashboardPanVerificationService = dashboardPanVerificationService,
            dashboardUdyamVerificationService = dashboardUdyamVerificationService,
            driverBackgroundVerificationService = driverBackgroundVerificationService,
            faceVerificationService = faceVerificationService,
            getBankAccount = getBankAccount,
            getDistances = getDistances,
            getDistancesForCancelRide = getDistancesForCancelRide,
            getDistancesForScheduledRides = getDistancesForScheduledRides,
            getEstimatedPickupDistances = getEstimatedPickupDistances,
            getExophone = getExophone,
            getPickupRoutes = getPickupRoutes,
            getPlaceDetails = getPlaceDetails,
            getPlaceName = getPlaceName,
            getRoutes = getRoutes,
            getTripRoutes = getTripRoutes,
            gstVerificationService = gstVerificationService,
            initiateCall = initiateCall,
            issueTicketService = issueTicketService,
            llmChatCompletion = Kernel.Prelude.fromMaybe ChatCompletion.Types.AzureOpenAI llmChatCompletion,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            panVerificationService = panVerificationService,
            rectifyDistantPointsFailure = rectifyDistantPointsFailure,
            retryBankAccountLink = retryBankAccountLink,
            sdkVerificationService = sdkVerificationService,
            sendSearchRequestToDriver = sendSearchRequestToDriver,
            smsProvidersPriorityList = smsProvidersPriorityList,
            snapToRoad = snapToRoad,
            snapToRoadProvidersList = snapToRoadProvidersList,
            totoVerificationPriorityList = totoVerificationPriorityList,
            udyamVerificationService = udyamVerificationService,
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
        Beam.backgroundVerification = backgroundVerification,
        Beam.createBankAccount = createBankAccount,
        Beam.createdAt = createdAt,
        Beam.dashboardGstVerificationService = dashboardGstVerificationService,
        Beam.dashboardPanVerificationService = dashboardPanVerificationService,
        Beam.dashboardUdyamVerificationService = dashboardUdyamVerificationService,
        Beam.driverBackgroundVerificationService = driverBackgroundVerificationService,
        Beam.faceVerificationService = faceVerificationService,
        Beam.getBankAccount = getBankAccount,
        Beam.getDistances = getDistances,
        Beam.getDistancesForCancelRide = getDistancesForCancelRide,
        Beam.getDistancesForScheduledRides = getDistancesForScheduledRides,
        Beam.getEstimatedPickupDistances = getEstimatedPickupDistances,
        Beam.getExophone = getExophone,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.getPlaceName = getPlaceName,
        Beam.getRoutes = getRoutes,
        Beam.getTripRoutes = getTripRoutes,
        Beam.gstVerificationService = gstVerificationService,
        Beam.initiateCall = initiateCall,
        Beam.issueTicketService = issueTicketService,
        Beam.llmChatCompletion = Kernel.Prelude.Just llmChatCompletion,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.panVerificationService = panVerificationService,
        Beam.rectifyDistantPointsFailure = rectifyDistantPointsFailure,
        Beam.retryBankAccountLink = retryBankAccountLink,
        Beam.sdkVerificationService = sdkVerificationService,
        Beam.sendSearchRequestToDriver = sendSearchRequestToDriver,
        Beam.smsProvidersPriorityList = smsProvidersPriorityList,
        Beam.snapToRoad = snapToRoad,
        Beam.snapToRoadProvidersList = snapToRoadProvidersList,
        Beam.totoVerificationPriorityList = totoVerificationPriorityList,
        Beam.udyamVerificationService = udyamVerificationService,
        Beam.updatedAt = updatedAt,
        Beam.verificationProvidersPriorityList = verificationProvidersPriorityList,
        Beam.verificationService = verificationService,
        Beam.whatsappProvidersPriorityList = whatsappProvidersPriorityList
      }
