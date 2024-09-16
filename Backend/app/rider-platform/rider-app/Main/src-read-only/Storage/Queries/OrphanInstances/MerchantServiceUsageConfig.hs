{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantServiceUsageConfig where

import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantServiceUsageConfig as Beam

instance FromTType' Beam.MerchantServiceUsageConfig Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig where
  fromTType' (Beam.MerchantServiceUsageConfigT {..}) = do
    pure $
      Just
        Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig
          { aadhaarVerificationService = aadhaarVerificationService,
            autoComplete = autoComplete,
            autoCompletePriorityList = fromMaybe [] autoCompletePriorityList,
            cancelPaymentIntent = fromMaybe Kernel.External.Payment.Types.Stripe cancelPaymentIntent,
            capturePaymentIntent = capturePaymentIntent,
            createEphemeralKeys = createEphemeralKeys,
            createPaymentCustomer = createPaymentCustomer,
            createPaymentIntent = createPaymentIntent,
            createSetupIntent = createSetupIntent,
            createdAt = createdAt,
            deleteCard = deleteCard,
            enableDashboardSms = enableDashboardSms,
            getCardList = getCardList,
            getDistances = getDistances,
            getDistancesForCancelRide = getDistancesForCancelRide,
            getDistancesForCancelRidePriorityList = fromMaybe [] getDistancesForCancelRidePriorityList,
            getDistancesForScheduledRides = getDistancesForScheduledRides,
            getDistancesForScheduledRidesPriorityList = fromMaybe [] getDistancesForScheduledRidesPriorityList,
            getDistancesPriorityList = fromMaybe [] getDistancesPriorityList,
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
            notifyPerson = notifyPerson,
            smsProvidersPriorityList = smsProvidersPriorityList,
            snapToRoad = snapToRoad,
            updateAmountInPaymentIntent = updateAmountInPaymentIntent,
            updatePaymentMethodInIntent = updatePaymentMethodInIntent,
            updatedAt = updatedAt,
            useFraudDetection = useFraudDetection,
            whatsappProvidersPriorityList = whatsappProvidersPriorityList
          }

instance ToTType' Beam.MerchantServiceUsageConfig Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig where
  toTType' (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig {..}) = do
    Beam.MerchantServiceUsageConfigT
      { Beam.aadhaarVerificationService = aadhaarVerificationService,
        Beam.autoComplete = autoComplete,
        Beam.autoCompletePriorityList = Kernel.Prelude.Just autoCompletePriorityList,
        Beam.cancelPaymentIntent = Kernel.Prelude.Just cancelPaymentIntent,
        Beam.capturePaymentIntent = capturePaymentIntent,
        Beam.createEphemeralKeys = createEphemeralKeys,
        Beam.createPaymentCustomer = createPaymentCustomer,
        Beam.createPaymentIntent = createPaymentIntent,
        Beam.createSetupIntent = createSetupIntent,
        Beam.createdAt = createdAt,
        Beam.deleteCard = deleteCard,
        Beam.enableDashboardSms = enableDashboardSms,
        Beam.getCardList = getCardList,
        Beam.getDistances = getDistances,
        Beam.getDistancesForCancelRide = getDistancesForCancelRide,
        Beam.getDistancesForCancelRidePriorityList = Kernel.Prelude.Just getDistancesForCancelRidePriorityList,
        Beam.getDistancesForScheduledRides = getDistancesForScheduledRides,
        Beam.getDistancesForScheduledRidesPriorityList = Kernel.Prelude.Just getDistancesForScheduledRidesPriorityList,
        Beam.getDistancesPriorityList = Kernel.Prelude.Just getDistancesPriorityList,
        Beam.getExophone = getExophone,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getPickupRoutesPriorityList = Kernel.Prelude.Just getPickupRoutesPriorityList,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.getPlaceDetailsPriorityList = Kernel.Prelude.Just getPlaceDetailsPriorityList,
        Beam.getPlaceName = getPlaceName,
        Beam.getPlaceNamePriorityList = Kernel.Prelude.Just getPlaceNamePriorityList,
        Beam.getRoutes = getRoutes,
        Beam.getRoutesPriorityList = Kernel.Prelude.Just getRoutesPriorityList,
        Beam.getTripRoutes = getTripRoutes,
        Beam.getTripRoutesPriorityList = Kernel.Prelude.Just getTripRoutesPriorityList,
        Beam.initiateCall = initiateCall,
        Beam.issueTicketService = issueTicketService,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.notifyPerson = notifyPerson,
        Beam.smsProvidersPriorityList = smsProvidersPriorityList,
        Beam.snapToRoad = snapToRoad,
        Beam.updateAmountInPaymentIntent = updateAmountInPaymentIntent,
        Beam.updatePaymentMethodInIntent = updatePaymentMethodInIntent,
        Beam.updatedAt = updatedAt,
        Beam.useFraudDetection = useFraudDetection,
        Beam.whatsappProvidersPriorityList = whatsappProvidersPriorityList
      }
