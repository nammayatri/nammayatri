{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantServiceUsageConfig where

import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Insurance.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Types
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
            getDistancesForScheduledRides = getDistancesForScheduledRides,
            getExophone = getExophone,
            getFirstPickupRoute = getFirstPickupRoute,
            getFrfsAutocompleteDistances = fromMaybe Kernel.External.Maps.Types.OSRM getFrfsAutocompleteDistances,
            getMultiModalService = fromMaybe Kernel.External.MultiModal.Types.OTPTransit getMultiModalService,
            getMultimodalWalkDistance = fromMaybe Kernel.External.Maps.Types.OSRM getMultimodalWalkDistance,
            getPickupRoutes = getPickupRoutes,
            getPlaceDetails = getPlaceDetails,
            getPlaceName = getPlaceName,
            getRoutes = getRoutes,
            getTripRoutes = getTripRoutes,
            initiateCall = initiateCall,
            insuranceService = fromMaybe Kernel.External.Insurance.Types.Acko insuranceService,
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
        Beam.getDistancesForScheduledRides = getDistancesForScheduledRides,
        Beam.getExophone = getExophone,
        Beam.getFirstPickupRoute = getFirstPickupRoute,
        Beam.getFrfsAutocompleteDistances = Kernel.Prelude.Just getFrfsAutocompleteDistances,
        Beam.getMultiModalService = Kernel.Prelude.Just getMultiModalService,
        Beam.getMultimodalWalkDistance = Kernel.Prelude.Just getMultimodalWalkDistance,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.getPlaceName = getPlaceName,
        Beam.getRoutes = getRoutes,
        Beam.getTripRoutes = getTripRoutes,
        Beam.initiateCall = initiateCall,
        Beam.insuranceService = Kernel.Prelude.Just insuranceService,
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
