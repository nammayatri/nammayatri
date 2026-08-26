{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceUsageConfig (module Storage.Queries.MerchantServiceUsageConfig, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceUsageConfig as Beam
import Storage.Queries.MerchantServiceUsageConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig))
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateSmsProvidersPriorityList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.External.SMS.Types.SmsService] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateSmsProvidersPriorityList smsProvidersPriorityList merchantOperatingCityId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.smsProvidersPriorityList smsProvidersPriorityList, Se.Set Beam.updatedAt _now] [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

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

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig -> m ())
updateByPrimaryKey (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarVerificationService aadhaarVerificationService,
      Se.Set Beam.additionalIssueTicketServices additionalIssueTicketServices,
      Se.Set Beam.autoComplete autoComplete,
      Se.Set Beam.cancelPaymentIntent (Kernel.Prelude.Just cancelPaymentIntent),
      Se.Set Beam.capturePaymentIntent capturePaymentIntent,
      Se.Set Beam.createEphemeralKeys createEphemeralKeys,
      Se.Set Beam.createPaymentCustomer createPaymentCustomer,
      Se.Set Beam.createPaymentIntent createPaymentIntent,
      Se.Set Beam.createPayoutOrder (Kernel.Prelude.Just createPayoutOrder),
      Se.Set Beam.createRefunds (Kernel.Prelude.Just createRefunds),
      Se.Set Beam.createSetupIntent createSetupIntent,
      Se.Set Beam.deleteCard deleteCard,
      Se.Set Beam.enableDashboardSms enableDashboardSms,
      Se.Set Beam.eventTrackingOverrides (Data.Aeson.toJSON <$> eventTrackingOverrides),
      Se.Set Beam.eventTrackingProviders (Kernel.Prelude.Just eventTrackingProviders),
      Se.Set Beam.getCardList getCardList,
      Se.Set Beam.getDistances getDistances,
      Se.Set Beam.getDistancesForCancelRide getDistancesForCancelRide,
      Se.Set Beam.getDistancesForScheduledRides getDistancesForScheduledRides,
      Se.Set Beam.getExophone getExophone,
      Se.Set Beam.getFirstPickupRoute getFirstPickupRoute,
      Se.Set Beam.getFrfsAutocompleteDistances (Kernel.Prelude.Just getFrfsAutocompleteDistances),
      Se.Set Beam.getInstructionRoute (Kernel.Prelude.Just getInstructionRoute),
      Se.Set Beam.getMultiModalService (Kernel.Prelude.Just getMultiModalService),
      Se.Set Beam.getMultimodalWalkDistance (Kernel.Prelude.Just getMultimodalWalkDistance),
      Se.Set Beam.getPickupRoutes getPickupRoutes,
      Se.Set Beam.getPlaceDetails getPlaceDetails,
      Se.Set Beam.getPlaceName getPlaceName,
      Se.Set Beam.getRefunds (Kernel.Prelude.Just getRefunds),
      Se.Set Beam.getRoutes getRoutes,
      Se.Set Beam.getTripRoutes getTripRoutes,
      Se.Set Beam.initiateCall initiateCall,
      Se.Set Beam.insuranceService (Kernel.Prelude.Just insuranceService),
      Se.Set Beam.issueTicketService issueTicketService,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.notifyPerson notifyPerson,
      Se.Set Beam.payoutOrderStatus (Kernel.Prelude.Just payoutOrderStatus),
      Se.Set Beam.smsProvidersPriorityList smsProvidersPriorityList,
      Se.Set Beam.snapToRoad snapToRoad,
      Se.Set Beam.sosTicketService sosTicketService,
      Se.Set Beam.updateAmountInPaymentIntent updateAmountInPaymentIntent,
      Se.Set Beam.updatePaymentMethodInIntent updatePaymentMethodInIntent,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.useFraudDetection useFraudDetection,
      Se.Set Beam.whatsappProvidersPriorityList whatsappProvidersPriorityList
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]
