{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SubscriptionConfig where

import qualified Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.SubscriptionConfig
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SubscriptionConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SubscriptionConfig.SubscriptionConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SubscriptionConfig.SubscriptionConfig] -> m ())
createMany = traverse_ create

findAllServicesUIEnabledByCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Bool -> m [Domain.Types.SubscriptionConfig.SubscriptionConfig])
findAllServicesUIEnabledByCity merchantOperatingCityId isUIEnabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.isUIEnabled $ Se.Eq (Kernel.Prelude.Just isUIEnabled)
        ]
    ]

findSubscriptionConfigsByMerchantOpCityIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Domain.Types.Plan.ServiceNames -> m (Maybe Domain.Types.SubscriptionConfig.SubscriptionConfig))
findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m (Maybe Domain.Types.SubscriptionConfig.SubscriptionConfig))
findByPrimaryKey serviceName merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SubscriptionConfig.SubscriptionConfig -> m ())
updateByPrimaryKey (Domain.Types.SubscriptionConfig.SubscriptionConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowDriverFeeCalcSchedule allowDriverFeeCalcSchedule,
      Se.Set Beam.allowDueAddition allowDueAddition,
      Se.Set Beam.allowManualPaymentLinks allowManualPaymentLinks,
      Se.Set Beam.autopayEnabled autopayEnabled,
      Se.Set Beam.cgstPercentageOneTimeSecurityDeposit cgstPercentageOneTimeSecurityDeposit,
      Se.Set Beam.dataEntityToSend (Kernel.Prelude.Just dataEntityToSend),
      Se.Set Beam.deepLinkExpiryTimeInMinutes deepLinkExpiryTimeInMinutes,
      Se.Set Beam.defaultCityVehicleCategory (Kernel.Prelude.Just defaultCityVehicleCategory),
      Se.Set Beam.disabledVariantsForSubscription disabledVariantsForSubscription,
      Se.Set Beam.enableCityBasedFeeSwitch (Kernel.Prelude.Just enableCityBasedFeeSwitch),
      Se.Set Beam.enableServiceUsageChargeDefault (Kernel.Prelude.Just enableServiceUsageChargeDefault),
      Se.Set Beam.eventsEnabledForWebhook (Kernel.Prelude.Just eventsEnabledForWebhook),
      Se.Set Beam.executionEnabledForVehicleCategories executionEnabledForVehicleCategories,
      Se.Set Beam.extWebhookConfigs (Kernel.Prelude.toJSON <$> extWebhookConfigs),
      Se.Set Beam.freeTrialRidesApplicable (Kernel.Prelude.Just freeTrialRidesApplicable),
      Se.Set Beam.genericBatchSizeForJobs genericBatchSizeForJobs,
      Se.Set Beam.genericJobRescheduleTime (Kernel.Utils.Common.nominalDiffTimeToSeconds genericJobRescheduleTime),
      Se.Set Beam.genericNextJobScheduleTimeThreshold (Kernel.Prelude.Just $ Kernel.Utils.Common.nominalDiffTimeToSeconds genericNextJobScheduleTimeThreshold),
      Se.Set Beam.isFreeTrialDaysApplicable (Kernel.Prelude.Just isFreeTrialDaysApplicable),
      Se.Set Beam.isSubscriptionEnabledAtCategoryLevel (Kernel.Prelude.Just isSubscriptionEnabledAtCategoryLevel),
      Se.Set Beam.isTriggeredAtEndRide isTriggeredAtEndRide,
      Se.Set Beam.isUIEnabled (Kernel.Prelude.Just isUIEnabled),
      Se.Set Beam.isVendorSplitEnabled isVendorSplitEnabled,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.numberOfFreeTrialRides numberOfFreeTrialRides,
      Se.Set Beam.partialDueClearanceMessageKey partialDueClearanceMessageKey,
      Se.Set Beam.paymentLinkChannel paymentLinkChannel,
      Se.Set Beam.paymentLinkJobTime (Kernel.Utils.Common.nominalDiffTimeToSeconds paymentLinkJobTime),
      Se.Set Beam.paymentServiceName paymentServiceName,
      Se.Set Beam.payoutServiceName payoutServiceName,
      Se.Set Beam.sendDeepLink sendDeepLink,
      Se.Set Beam.sendInAppFcmNotifications sendInAppFcmNotifications,
      Se.Set Beam.sgstPercentageOneTimeSecurityDeposit sgstPercentageOneTimeSecurityDeposit,
      Se.Set Beam.showManualPlansInUI showManualPlansInUI,
      Se.Set Beam.subscriptionDown subscriptionDown,
      Se.Set Beam.subscriptionEnabledForVehicleCategories subscriptionEnabledForVehicleCategories,
      Se.Set Beam.useOverlayService useOverlayService,
      Se.Set Beam.waiveOffOfferDescription (Kernel.Prelude.Just waiveOffOfferDescription),
      Se.Set Beam.waiveOffOfferTitle (Kernel.Prelude.Just waiveOffOfferTitle),
      Se.Set Beam.webhookConfig (Kernel.Prelude.toJSON <$> webhookConfig),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.serviceName $ Se.Eq serviceName, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]]

instance FromTType' Beam.SubscriptionConfig Domain.Types.SubscriptionConfig.SubscriptionConfig where
  fromTType' (Beam.SubscriptionConfigT {..}) = do
    pure $
      Just
        Domain.Types.SubscriptionConfig.SubscriptionConfig
          { allowDriverFeeCalcSchedule = allowDriverFeeCalcSchedule,
            allowDueAddition = allowDueAddition,
            allowManualPaymentLinks = allowManualPaymentLinks,
            autopayEnabled = autopayEnabled,
            cgstPercentageOneTimeSecurityDeposit = cgstPercentageOneTimeSecurityDeposit,
            dataEntityToSend = Kernel.Prelude.fromMaybe [] dataEntityToSend,
            deepLinkExpiryTimeInMinutes = deepLinkExpiryTimeInMinutes,
            defaultCityVehicleCategory = Kernel.Prelude.fromMaybe Domain.Types.VehicleCategory.AUTO_CATEGORY defaultCityVehicleCategory,
            disabledVariantsForSubscription = disabledVariantsForSubscription,
            enableCityBasedFeeSwitch = Kernel.Prelude.fromMaybe False enableCityBasedFeeSwitch,
            enableServiceUsageChargeDefault = Kernel.Prelude.fromMaybe True enableServiceUsageChargeDefault,
            eventsEnabledForWebhook = Kernel.Prelude.fromMaybe [] eventsEnabledForWebhook,
            executionEnabledForVehicleCategories = executionEnabledForVehicleCategories,
            extWebhookConfigs = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< extWebhookConfigs,
            freeTrialRidesApplicable = Kernel.Prelude.fromMaybe False freeTrialRidesApplicable,
            genericBatchSizeForJobs = genericBatchSizeForJobs,
            genericJobRescheduleTime = Kernel.Utils.Common.secondsToNominalDiffTime genericJobRescheduleTime,
            genericNextJobScheduleTimeThreshold = Kernel.Prelude.fromMaybe 300 (Kernel.Utils.Common.secondsToNominalDiffTime <$> genericNextJobScheduleTimeThreshold),
            isFreeTrialDaysApplicable = Kernel.Prelude.fromMaybe True isFreeTrialDaysApplicable,
            isSubscriptionEnabledAtCategoryLevel = Kernel.Prelude.fromMaybe False isSubscriptionEnabledAtCategoryLevel,
            isTriggeredAtEndRide = isTriggeredAtEndRide,
            isUIEnabled = Kernel.Prelude.fromMaybe False isUIEnabled,
            isVendorSplitEnabled = isVendorSplitEnabled,
            maxRetryCount = maxRetryCount,
            numberOfFreeTrialRides = numberOfFreeTrialRides,
            partialDueClearanceMessageKey = partialDueClearanceMessageKey,
            paymentLinkChannel = paymentLinkChannel,
            paymentLinkJobTime = Kernel.Utils.Common.secondsToNominalDiffTime paymentLinkJobTime,
            paymentServiceName = paymentServiceName,
            payoutServiceName = payoutServiceName,
            sendDeepLink = sendDeepLink,
            sendInAppFcmNotifications = sendInAppFcmNotifications,
            serviceName = serviceName,
            sgstPercentageOneTimeSecurityDeposit = sgstPercentageOneTimeSecurityDeposit,
            showManualPlansInUI = showManualPlansInUI,
            subscriptionDown = subscriptionDown,
            subscriptionEnabledForVehicleCategories = subscriptionEnabledForVehicleCategories,
            useOverlayService = useOverlayService,
            waiveOffOfferDescription = Kernel.Prelude.fromMaybe "NAN" waiveOffOfferDescription,
            waiveOffOfferTitle = Kernel.Prelude.fromMaybe "NAN" waiveOffOfferTitle,
            webhookConfig = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< webhookConfig,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SubscriptionConfig Domain.Types.SubscriptionConfig.SubscriptionConfig where
  toTType' (Domain.Types.SubscriptionConfig.SubscriptionConfig {..}) = do
    Beam.SubscriptionConfigT
      { Beam.allowDriverFeeCalcSchedule = allowDriverFeeCalcSchedule,
        Beam.allowDueAddition = allowDueAddition,
        Beam.allowManualPaymentLinks = allowManualPaymentLinks,
        Beam.autopayEnabled = autopayEnabled,
        Beam.cgstPercentageOneTimeSecurityDeposit = cgstPercentageOneTimeSecurityDeposit,
        Beam.dataEntityToSend = Kernel.Prelude.Just dataEntityToSend,
        Beam.deepLinkExpiryTimeInMinutes = deepLinkExpiryTimeInMinutes,
        Beam.defaultCityVehicleCategory = Kernel.Prelude.Just defaultCityVehicleCategory,
        Beam.disabledVariantsForSubscription = disabledVariantsForSubscription,
        Beam.enableCityBasedFeeSwitch = Kernel.Prelude.Just enableCityBasedFeeSwitch,
        Beam.enableServiceUsageChargeDefault = Kernel.Prelude.Just enableServiceUsageChargeDefault,
        Beam.eventsEnabledForWebhook = Kernel.Prelude.Just eventsEnabledForWebhook,
        Beam.executionEnabledForVehicleCategories = executionEnabledForVehicleCategories,
        Beam.extWebhookConfigs = Kernel.Prelude.toJSON <$> extWebhookConfigs,
        Beam.freeTrialRidesApplicable = Kernel.Prelude.Just freeTrialRidesApplicable,
        Beam.genericBatchSizeForJobs = genericBatchSizeForJobs,
        Beam.genericJobRescheduleTime = Kernel.Utils.Common.nominalDiffTimeToSeconds genericJobRescheduleTime,
        Beam.genericNextJobScheduleTimeThreshold = Kernel.Prelude.Just $ Kernel.Utils.Common.nominalDiffTimeToSeconds genericNextJobScheduleTimeThreshold,
        Beam.isFreeTrialDaysApplicable = Kernel.Prelude.Just isFreeTrialDaysApplicable,
        Beam.isSubscriptionEnabledAtCategoryLevel = Kernel.Prelude.Just isSubscriptionEnabledAtCategoryLevel,
        Beam.isTriggeredAtEndRide = isTriggeredAtEndRide,
        Beam.isUIEnabled = Kernel.Prelude.Just isUIEnabled,
        Beam.isVendorSplitEnabled = isVendorSplitEnabled,
        Beam.maxRetryCount = maxRetryCount,
        Beam.numberOfFreeTrialRides = numberOfFreeTrialRides,
        Beam.partialDueClearanceMessageKey = partialDueClearanceMessageKey,
        Beam.paymentLinkChannel = paymentLinkChannel,
        Beam.paymentLinkJobTime = Kernel.Utils.Common.nominalDiffTimeToSeconds paymentLinkJobTime,
        Beam.paymentServiceName = paymentServiceName,
        Beam.payoutServiceName = payoutServiceName,
        Beam.sendDeepLink = sendDeepLink,
        Beam.sendInAppFcmNotifications = sendInAppFcmNotifications,
        Beam.serviceName = serviceName,
        Beam.sgstPercentageOneTimeSecurityDeposit = sgstPercentageOneTimeSecurityDeposit,
        Beam.showManualPlansInUI = showManualPlansInUI,
        Beam.subscriptionDown = subscriptionDown,
        Beam.subscriptionEnabledForVehicleCategories = subscriptionEnabledForVehicleCategories,
        Beam.useOverlayService = useOverlayService,
        Beam.waiveOffOfferDescription = Kernel.Prelude.Just waiveOffOfferDescription,
        Beam.waiveOffOfferTitle = Kernel.Prelude.Just waiveOffOfferTitle,
        Beam.webhookConfig = Kernel.Prelude.toJSON <$> webhookConfig,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
