{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantServiceUsageConfig where

import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantServiceUsageConfig as Beam

instance FromTType' Beam.MerchantServiceUsageConfig Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig where
  fromTType' (Beam.MerchantServiceUsageConfigT {..}) = do
    pure $
      Just
        Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig
          { merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            initiateCall = initiateCall,
            notifyPerson = notifyPerson,
            getDistances = getDistances,
            getRoutes = getRoutes,
            snapToRoad = snapToRoad,
            getPlaceName = getPlaceName,
            getPickupRoutes = getPickupRoutes,
            getTripRoutes = getTripRoutes,
            getPlaceDetails = getPlaceDetails,
            autoComplete = autoComplete,
            aadhaarVerificationService = aadhaarVerificationService,
            getDistancesForCancelRide = getDistancesForCancelRide,
            smsProvidersPriorityList = smsProvidersPriorityList,
            whatsappProvidersPriorityList = whatsappProvidersPriorityList,
            issueTicketService = issueTicketService,
            useFraudDetection = useFraudDetection,
            enableDashboardSms = enableDashboardSms,
            getExophone = getExophone,
            createPaymentCustomer = createPaymentCustomer,
            createEphemeralKeys = createEphemeralKeys,
            getCardList = getCardList,
            createPaymentIntent = createPaymentIntent,
            updatePaymentMethodInIntent = updatePaymentMethodInIntent,
            capturePaymentIntent = capturePaymentIntent,
            updateAmountInPaymentIntent = updateAmountInPaymentIntent,
            createSetupIntent = createSetupIntent,
            deleteCard = deleteCard,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' Beam.MerchantServiceUsageConfig Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig where
  toTType' (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig {..}) = do
    Beam.MerchantServiceUsageConfigT
      { Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.initiateCall = initiateCall,
        Beam.notifyPerson = notifyPerson,
        Beam.getDistances = getDistances,
        Beam.getRoutes = getRoutes,
        Beam.snapToRoad = snapToRoad,
        Beam.getPlaceName = getPlaceName,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getTripRoutes = getTripRoutes,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.autoComplete = autoComplete,
        Beam.aadhaarVerificationService = aadhaarVerificationService,
        Beam.getDistancesForCancelRide = getDistancesForCancelRide,
        Beam.smsProvidersPriorityList = smsProvidersPriorityList,
        Beam.whatsappProvidersPriorityList = whatsappProvidersPriorityList,
        Beam.issueTicketService = issueTicketService,
        Beam.useFraudDetection = useFraudDetection,
        Beam.enableDashboardSms = enableDashboardSms,
        Beam.getExophone = getExophone,
        Beam.createPaymentCustomer = createPaymentCustomer,
        Beam.createEphemeralKeys = createEphemeralKeys,
        Beam.getCardList = getCardList,
        Beam.createPaymentIntent = createPaymentIntent,
        Beam.updatePaymentMethodInIntent = updatePaymentMethodInIntent,
        Beam.capturePaymentIntent = capturePaymentIntent,
        Beam.updateAmountInPaymentIntent = updateAmountInPaymentIntent,
        Beam.createSetupIntent = createSetupIntent,
        Beam.deleteCard = deleteCard,
        Beam.updatedAt = updatedAt,
        Beam.createdAt = createdAt
      }
