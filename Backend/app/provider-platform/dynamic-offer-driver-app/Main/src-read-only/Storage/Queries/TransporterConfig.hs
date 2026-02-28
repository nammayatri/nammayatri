{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.TransporterConfig (module Storage.Queries.TransporterConfig, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.TransporterConfigExtra as ReExport
import qualified Domain.Types.TransporterConfig
import qualified Storage.Beam.TransporterConfig as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Utils.Common
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TransporterConfig.TransporterConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TransporterConfig.TransporterConfig] -> m ())
createMany = traverse_ create
findByMerchantOpCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                          (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.TransporterConfig.TransporterConfig))
findByMerchantOpCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
findExotelAppletMappingByMOCID :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.TransporterConfig.TransporterConfig))
findExotelAppletMappingByMOCID merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
update :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TransporterConfig.TransporterConfig -> m ())
update (Domain.Types.TransporterConfig.TransporterConfig {..}) = do {_now <- getCurrentTime;
                                                                     updateOneWithKV [Se.Set Beam.pickupLocThreshold pickupLocThreshold,
                                                                                      Se.Set Beam.dropLocThreshold dropLocThreshold,
                                                                                      Se.Set Beam.rideTimeEstimatedThreshold rideTimeEstimatedThreshold,
                                                                                      Se.Set Beam.defaultPopupDelay defaultPopupDelay,
                                                                                      Se.Set Beam.popupDelayToAddAsPenalty popupDelayToAddAsPenalty,
                                                                                      Se.Set Beam.fleetAlertThreshold fleetAlertThreshold,
                                                                                      Se.Set Beam.thresholdCancellationScore thresholdCancellationScore,
                                                                                      Se.Set Beam.minRidesForCancellationScore minRidesForCancellationScore,
                                                                                      Se.Set Beam.mediaFileUrlPattern mediaFileUrlPattern,
                                                                                      Se.Set Beam.mediaFileSizeUpperLimit mediaFileSizeUpperLimit,
                                                                                      Se.Set Beam.onboardingTryLimit onboardingTryLimit,
                                                                                      Se.Set Beam.onboardingRetryTimeInHours onboardingRetryTimeInHours,
                                                                                      Se.Set Beam.checkImageExtractionForDashboard checkImageExtractionForDashboard,
                                                                                      Se.Set Beam.searchRepeatLimit searchRepeatLimit,
                                                                                      Se.Set Beam.scheduledRideSearchRepeatLimit (Just scheduledRideSearchRepeatLimit),
                                                                                      Se.Set Beam.driverPaymentCycleStartTime (Kernel.Utils.Common.nominalDiffTimeToSeconds driverPaymentCycleStartTime),
                                                                                      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
                                                                                      Se.Set Beam.driverPaymentCycleBuffer (Kernel.Utils.Common.nominalDiffTimeToSeconds driverPaymentCycleBuffer),
                                                                                      Se.Set Beam.driverPaymentReminderInterval (Kernel.Utils.Common.nominalDiffTimeToSeconds driverPaymentReminderInterval),
                                                                                      Se.Set Beam.driverPaymentCycleDuration (Kernel.Utils.Common.nominalDiffTimeToSeconds driverPaymentCycleDuration),
                                                                                      Se.Set Beam.driverAutoPayNotificationTime (Kernel.Utils.Common.nominalDiffTimeToSeconds driverAutoPayNotificationTime),
                                                                                      Se.Set Beam.driverAutoPayExecutionTime (Kernel.Utils.Common.nominalDiffTimeToSeconds driverAutoPayExecutionTime),
                                                                                      Se.Set Beam.driverFeeMandateNotificationBatchSize driverFeeMandateNotificationBatchSize,
                                                                                      Se.Set Beam.driverFeeMandateExecutionBatchSize driverFeeMandateExecutionBatchSize,
                                                                                      Se.Set Beam.mandateNotificationRescheduleInterval (Kernel.Utils.Common.nominalDiffTimeToSeconds mandateNotificationRescheduleInterval),
                                                                                      Se.Set Beam.mandateExecutionRescheduleInterval (Kernel.Utils.Common.nominalDiffTimeToSeconds mandateExecutionRescheduleInterval),
                                                                                      Se.Set Beam.driverFeeCalculationTime (Kernel.Utils.Common.nominalDiffTimeToSeconds <$> driverFeeCalculationTime),
                                                                                      Se.Set Beam.driverFeeCalculatorBatchSize driverFeeCalculatorBatchSize,
                                                                                      Se.Set Beam.driverFeeCalculatorBatchGap (Kernel.Utils.Common.nominalDiffTimeToSeconds <$> driverFeeCalculatorBatchGap),
                                                                                      Se.Set Beam.orderAndNotificationStatusCheckTime (Kernel.Utils.Common.nominalDiffTimeToSeconds orderAndNotificationStatusCheckTime),
                                                                                      Se.Set Beam.orderAndNotificationStatusCheckTimeLimit (Kernel.Utils.Common.nominalDiffTimeToSeconds orderAndNotificationStatusCheckTimeLimit),
                                                                                      Se.Set Beam.snapToRoadConfidenceThreshold snapToRoadConfidenceThreshold,
                                                                                      Se.Set Beam.useWithSnapToRoadFallback useWithSnapToRoadFallback,
                                                                                      Se.Set Beam.dpGeoHashPercision dpGeoHashPercision,
                                                                                      Se.Set Beam.updatedAt _now] [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]}



