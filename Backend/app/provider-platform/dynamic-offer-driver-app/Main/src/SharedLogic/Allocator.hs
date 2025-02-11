{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module SharedLogic.Allocator where

import Data.Singletons.TH
import qualified Domain.Action.WebhookHandler as AWebhook
import qualified Domain.Types.ApprovalRequest as DTR
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Message as DMessage
import Domain.Types.Overlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as Plan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler
import qualified Lib.Yudhishthira.Types as LYT
import qualified Tools.Notifications as Notify

data AllocatorJobType
  = SendSearchRequestToDriver
  | UnblockDriver
  | UnblockSoftBlockedDriver
  | SoftBlockNotifyDriver
  | SupplyDemand
  | SendPDNNotificationToDriver
  | MandateExecution
  | CalculateDriverFees
  | OrderAndNotificationStatusUpdate
  | SendOverlay
  | BadDebtCalculation
  | SendManualPaymentLink
  | RetryDocumentVerification
  | ScheduledRideNotificationsToDriver
  | DriverReferralPayout
  | ScheduledRideAssignedOnUpdate
  | CheckExotelCallStatusAndNotifyBAP
  | Daily
  | Weekly
  | Monthly
  | Quarterly
  | DailyUpdateTag
  | WeeklyUpdateTag
  | MonthlyUpdateTag
  | QuarterlyUpdateTag
  | FleetAlert
  | SendWebhookToExternal
  | ScheduledFCMS
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''AllocatorJobType]
showSingInstance ''AllocatorJobType

instance JobProcessor AllocatorJobType where
  type MerchantType AllocatorJobType = DM.Merchant
  type MerchantOperatingCityType AllocatorJobType = DMOC.MerchantOperatingCity
  restoreAnyJobInfo :: Sing (e :: AllocatorJobType) -> Text -> Maybe (AnyJobInfo AllocatorJobType)
  restoreAnyJobInfo SSendSearchRequestToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendSearchRequestToDriver jobData
  restoreAnyJobInfo SUnblockDriver jobData = AnyJobInfo <$> restoreJobInfo SUnblockDriver jobData
  restoreAnyJobInfo SUnblockSoftBlockedDriver jobData = AnyJobInfo <$> restoreJobInfo SUnblockSoftBlockedDriver jobData
  restoreAnyJobInfo SSoftBlockNotifyDriver jobData = AnyJobInfo <$> restoreJobInfo SSoftBlockNotifyDriver jobData
  restoreAnyJobInfo SSupplyDemand jobData = AnyJobInfo <$> restoreJobInfo SSupplyDemand jobData
  restoreAnyJobInfo SSendPDNNotificationToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendPDNNotificationToDriver jobData
  restoreAnyJobInfo SMandateExecution jobData = AnyJobInfo <$> restoreJobInfo SMandateExecution jobData
  restoreAnyJobInfo SCalculateDriverFees jobData = AnyJobInfo <$> restoreJobInfo SCalculateDriverFees jobData
  restoreAnyJobInfo SOrderAndNotificationStatusUpdate jobData = AnyJobInfo <$> restoreJobInfo SOrderAndNotificationStatusUpdate jobData
  restoreAnyJobInfo SSendOverlay jobData = AnyJobInfo <$> restoreJobInfo SSendOverlay jobData
  restoreAnyJobInfo SBadDebtCalculation jobData = AnyJobInfo <$> restoreJobInfo SBadDebtCalculation jobData
  restoreAnyJobInfo SSendManualPaymentLink jobData = AnyJobInfo <$> restoreJobInfo SSendManualPaymentLink jobData
  restoreAnyJobInfo SRetryDocumentVerification jobData = AnyJobInfo <$> restoreJobInfo SRetryDocumentVerification jobData
  restoreAnyJobInfo SScheduledRideNotificationsToDriver jobData = AnyJobInfo <$> restoreJobInfo SScheduledRideNotificationsToDriver jobData
  restoreAnyJobInfo SDriverReferralPayout jobData = AnyJobInfo <$> restoreJobInfo SDriverReferralPayout jobData
  restoreAnyJobInfo SScheduledRideAssignedOnUpdate jobData = AnyJobInfo <$> restoreJobInfo SScheduledRideAssignedOnUpdate jobData
  restoreAnyJobInfo SCheckExotelCallStatusAndNotifyBAP jobData = AnyJobInfo <$> restoreJobInfo SCheckExotelCallStatusAndNotifyBAP jobData
  restoreAnyJobInfo SDaily jobData = AnyJobInfo <$> restoreJobInfo SDaily jobData
  restoreAnyJobInfo SWeekly jobData = AnyJobInfo <$> restoreJobInfo SWeekly jobData
  restoreAnyJobInfo SMonthly jobData = AnyJobInfo <$> restoreJobInfo SMonthly jobData
  restoreAnyJobInfo SQuarterly jobData = AnyJobInfo <$> restoreJobInfo SQuarterly jobData
  restoreAnyJobInfo SDailyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SDailyUpdateTag jobData
  restoreAnyJobInfo SWeeklyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SWeeklyUpdateTag jobData
  restoreAnyJobInfo SMonthlyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SMonthlyUpdateTag jobData
  restoreAnyJobInfo SQuarterlyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SQuarterlyUpdateTag jobData
  restoreAnyJobInfo SFleetAlert jobData = AnyJobInfo <$> restoreJobInfo SFleetAlert jobData
  restoreAnyJobInfo SSendWebhookToExternal jobData = AnyJobInfo <$> restoreJobInfo SSendWebhookToExternal jobData
  restoreAnyJobInfo SScheduledFCMS jobData = AnyJobInfo <$> restoreJobInfo SScheduledFCMS jobData

instance JobInfoProcessor 'Daily

instance JobInfoProcessor 'Weekly

instance JobInfoProcessor 'Monthly

instance JobInfoProcessor 'Quarterly

instance JobInfoProcessor 'DailyUpdateTag

instance JobInfoProcessor 'WeeklyUpdateTag

instance JobInfoProcessor 'MonthlyUpdateTag

instance JobInfoProcessor 'QuarterlyUpdateTag

type instance JobContent 'Daily = LYT.KaalChakraJobData

type instance JobContent 'Weekly = LYT.KaalChakraJobData

type instance JobContent 'Monthly = LYT.KaalChakraJobData

type instance JobContent 'Quarterly = LYT.KaalChakraJobData

type instance JobContent 'DailyUpdateTag = LYT.UpdateKaalBasedTagsData

type instance JobContent 'WeeklyUpdateTag = LYT.UpdateKaalBasedTagsData

type instance JobContent 'MonthlyUpdateTag = LYT.UpdateKaalBasedTagsData

type instance JobContent 'QuarterlyUpdateTag = LYT.UpdateKaalBasedTagsData

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { searchTryId :: Id DST.SearchTry,
    estimatedRideDistance :: Maybe Meters
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendSearchRequestToDriver

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

newtype UnblockDriverRequestJobData = UnblockDriverRequestJobData
  { driverId :: Id DP.Driver
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnblockDriver

type instance JobContent 'UnblockDriver = UnblockDriverRequestJobData

data FleetAlertJobData = FleetAlertJobData
  { fleetOwnerId :: Id DP.Driver,
    entityId :: Id DTR.ApprovalRequest,
    appletId :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'FleetAlert

type instance JobContent 'FleetAlert = FleetAlertJobData

data ScheduledFCMSJobData = ScheduledFCMSJobData
  { driverIds :: [Id DP.Driver],
    message :: DMessage.RawMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledFCMS

type instance JobContent 'ScheduledFCMS = ScheduledFCMSJobData

newtype UnblockSoftBlockedDriverRequestJobData = UnblockSoftBlockedDriverRequestJobData
  { driverId :: Id DP.Driver
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnblockSoftBlockedDriver

type instance JobContent 'UnblockSoftBlockedDriver = UnblockSoftBlockedDriverRequestJobData

data SoftBlockNotifyDriverRequestJobData = SoftBlockNotifyDriverRequestJobData
  { driverId :: Id DP.Driver,
    pendingNotificationRedisKey :: Text,
    entityData :: Notify.IssueBreachEntityData
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SoftBlockNotifyDriver

type instance JobContent 'SoftBlockNotifyDriver = SoftBlockNotifyDriverRequestJobData

data SupplyDemandRequestJobData = SupplyDemandRequestJobData
  { scheduleTimeIntervalInMin :: Int,
    supplyDemandRatioTTLInSec :: Int,
    calculationDataIntervalInMin :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SupplyDemand

type instance JobContent 'SupplyDemand = SupplyDemandRequestJobData

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

data SendPDNNotificationToDriverJobData = SendPDNNotificationToDriverJobData
  { startTime :: UTCTime,
    endTime :: UTCTime,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    retryCount :: Maybe Int,
    serviceName :: Maybe Plan.ServiceNames
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendPDNNotificationToDriver

type instance JobContent 'SendPDNNotificationToDriver = SendPDNNotificationToDriverJobData

data MandateExecutionInfo = MandateExecutionInfo
  { startTime :: UTCTime,
    endTime :: UTCTime,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    serviceName :: Maybe Plan.ServiceNames
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'MandateExecution

type instance JobContent 'MandateExecution = MandateExecutionInfo

data CalculateDriverFeesJobData = CalculateDriverFeesJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    startTime :: UTCTime,
    endTime :: UTCTime,
    serviceName :: Maybe Plan.ServiceNames,
    scheduleNotification :: Maybe Bool,
    scheduleOverlay :: Maybe Bool,
    scheduleManualPaymentLink :: Maybe Bool,
    scheduleDriverFeeCalc :: Maybe Bool,
    createChildJobs :: Maybe Bool,
    recalculateManualReview :: Maybe Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CalculateDriverFees

type instance JobContent 'CalculateDriverFees = CalculateDriverFeesJobData

data RetryDocumentVerificationJobData = RetryDocumentVerificationJobData
  { requestId :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'RetryDocumentVerification

type instance JobContent 'RetryDocumentVerification = RetryDocumentVerificationJobData

data OrderAndNotificationStatusUpdateJobData = OrderAndNotificationStatusUpdateJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'OrderAndNotificationStatusUpdate

type instance JobContent 'OrderAndNotificationStatusUpdate = OrderAndNotificationStatusUpdateJobData

data SendOverlayJobData = SendOverlayJobData
  { merchantId :: Id DM.Merchant,
    overlayKey :: Text,
    udf1 :: Maybe Text,
    condition :: OverlayCondition,
    rescheduleInterval :: Maybe Seconds,
    scheduledTime :: TimeOfDay,
    freeTrialDays :: Int,
    timeDiffFromUtc :: Seconds,
    driverPaymentCycleDuration :: NominalDiffTime,
    driverPaymentCycleStartTime :: NominalDiffTime,
    driverFeeOverlaySendingTimeLimitInDays :: Int,
    overlayBatchSize :: Int,
    serviceName :: Maybe Plan.ServiceNames,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    vehicleCategory :: Maybe DVC.VehicleCategory
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendOverlay

type instance JobContent 'SendOverlay = SendOverlayJobData

data BadDebtCalculationJobData = BadDebtCalculationJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'BadDebtCalculation

type instance JobContent 'BadDebtCalculation = BadDebtCalculationJobData

data SendManualPaymentLinkJobData = SendManualPaymentLinkJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    serviceName :: Plan.ServiceNames,
    startTime :: UTCTime,
    endTime :: UTCTime,
    channel :: MediaChannel
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendManualPaymentLink

type instance JobContent 'SendManualPaymentLink = SendManualPaymentLinkJobData

data ScheduledRideNotificationsToDriverJobData = ScheduledRideNotificationsToDriverJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    timeDiffEvent :: DRN.TimeDiffEvent,
    bookingStatus :: DB.BookingStatus,
    notificationType :: DRN.NotificationType,
    notificationKey :: Text,
    onlyIfOffline :: Bool,
    bookingId :: Id DB.Booking,
    driverId :: Id DP.Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledRideNotificationsToDriver

type instance JobContent 'ScheduledRideNotificationsToDriver = ScheduledRideNotificationsToDriverJobData

data DriverReferralPayoutJobData = DriverReferralPayoutJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    toScheduleNextPayout :: Bool,
    statusForRetry :: DS.PayoutStatus,
    schedulePayoutForDay :: Maybe Integer
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'DriverReferralPayout

type instance JobContent 'DriverReferralPayout = DriverReferralPayoutJobData

data ScheduledRideAssignedOnUpdateJobData = ScheduledRideAssignedOnUpdateJobData
  { bookingId :: Id DB.Booking,
    rideId :: Id SRide.Ride,
    driverId :: Id DP.Person
  }
  deriving (Generic, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledRideAssignedOnUpdate

type instance JobContent 'ScheduledRideAssignedOnUpdate = ScheduledRideAssignedOnUpdateJobData

data CheckExotelCallStatusAndNotifyBAPJobData = CheckExotelCallStatusAndNotifyBAPJobData
  { rideId :: Id DRide.Ride,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }
  deriving (Generic, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckExotelCallStatusAndNotifyBAP

type instance JobContent 'CheckExotelCallStatusAndNotifyBAP = CheckExotelCallStatusAndNotifyBAPJobData

data SendWebhookToExternalJobData = SendWebhookToExternalJobData
  { webhookData :: AWebhook.WebhookJobInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance JobInfoProcessor 'SendWebhookToExternal

type instance JobContent 'SendWebhookToExternal = SendWebhookToExternalJobData
