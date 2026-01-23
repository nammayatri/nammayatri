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

module SharedLogic.JobScheduler where

import Data.Singletons.TH
import Domain.Types.Booking
import qualified Domain.Types.BookingStatus as DEB
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.NyRegularSubscription as NyRegularSubscription
import Domain.Types.Person
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler
import qualified Lib.Yudhishthira.Types as LYT

data RiderJobType
  = CheckPNAndSendSMS
  | ScheduledRideNotificationsToRider
  | ScheduleTagActionNotification
  | SafetyIVR
  | CallPoliceApi
  | CheckExotelCallStatusAndNotifyBPP
  | SafetyCSAlert
  | ExecutePaymentIntent
  | CancelExecutePaymentIntent
  | OtherJobTypes
  | MetroIncentivePayout
  | ScheduledRidePopupToRider
  | Daily
  | Weekly
  | Monthly
  | Quarterly
  | DailyUpdateTag
  | WeeklyUpdateTag
  | MonthlyUpdateTag
  | QuarterlyUpdateTag
  | PostRideSafetyNotification
  | UpdateCrisUtsData
  | CheckMultimodalConfirmFail
  | CheckRefundStatus
  | MetroBusinessHour
  | NyRegularMaster
  | NyRegularInstance
  | CrisRecon
  | PaymentOrderStatusCheck
  | PartnerInvoiceDataExport
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''RiderJobType]
showSingInstance ''RiderJobType

instance JobProcessor RiderJobType where
  type MerchantType RiderJobType = DM.Merchant
  type MerchantOperatingCityType RiderJobType = DMOC.MerchantOperatingCity
  restoreAnyJobInfo :: Sing (e :: RiderJobType) -> Text -> Maybe (AnyJobInfo RiderJobType)
  restoreAnyJobInfo SCheckPNAndSendSMS jobData = AnyJobInfo <$> restoreJobInfo SCheckPNAndSendSMS jobData
  restoreAnyJobInfo SScheduledRideNotificationsToRider jobData = AnyJobInfo <$> restoreJobInfo SScheduledRideNotificationsToRider jobData
  restoreAnyJobInfo SScheduleTagActionNotification jobData = AnyJobInfo <$> restoreJobInfo SScheduleTagActionNotification jobData
  restoreAnyJobInfo SSafetyIVR jobData = AnyJobInfo <$> restoreJobInfo SSafetyIVR jobData
  restoreAnyJobInfo SCallPoliceApi jobData = AnyJobInfo <$> restoreJobInfo SCallPoliceApi jobData
  restoreAnyJobInfo SSafetyCSAlert jobData = AnyJobInfo <$> restoreJobInfo SSafetyCSAlert jobData
  restoreAnyJobInfo SCheckExotelCallStatusAndNotifyBPP jobData = AnyJobInfo <$> restoreJobInfo SCheckExotelCallStatusAndNotifyBPP jobData
  restoreAnyJobInfo SOtherJobTypes jobData = AnyJobInfo <$> restoreJobInfo SOtherJobTypes jobData
  restoreAnyJobInfo SMetroIncentivePayout jobData = AnyJobInfo <$> restoreJobInfo SMetroIncentivePayout jobData
  restoreAnyJobInfo SScheduledRidePopupToRider jobData = AnyJobInfo <$> restoreJobInfo SScheduledRidePopupToRider jobData
  restoreAnyJobInfo SExecutePaymentIntent jobData = AnyJobInfo <$> restoreJobInfo SExecutePaymentIntent jobData
  restoreAnyJobInfo SCancelExecutePaymentIntent jobData = AnyJobInfo <$> restoreJobInfo SCancelExecutePaymentIntent jobData
  restoreAnyJobInfo SDaily jobData = AnyJobInfo <$> restoreJobInfo SDaily jobData
  restoreAnyJobInfo SWeekly jobData = AnyJobInfo <$> restoreJobInfo SWeekly jobData
  restoreAnyJobInfo SMonthly jobData = AnyJobInfo <$> restoreJobInfo SMonthly jobData
  restoreAnyJobInfo SQuarterly jobData = AnyJobInfo <$> restoreJobInfo SQuarterly jobData
  restoreAnyJobInfo SDailyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SDailyUpdateTag jobData
  restoreAnyJobInfo SWeeklyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SWeeklyUpdateTag jobData
  restoreAnyJobInfo SMonthlyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SMonthlyUpdateTag jobData
  restoreAnyJobInfo SQuarterlyUpdateTag jobData = AnyJobInfo <$> restoreJobInfo SQuarterlyUpdateTag jobData
  restoreAnyJobInfo SPostRideSafetyNotification jobData = AnyJobInfo <$> restoreJobInfo SPostRideSafetyNotification jobData
  restoreAnyJobInfo SUpdateCrisUtsData jobData = AnyJobInfo <$> restoreJobInfo SUpdateCrisUtsData jobData
  restoreAnyJobInfo SCheckMultimodalConfirmFail jobData = AnyJobInfo <$> restoreJobInfo SCheckMultimodalConfirmFail jobData
  restoreAnyJobInfo SCheckRefundStatus jobData = AnyJobInfo <$> restoreJobInfo SCheckRefundStatus jobData
  restoreAnyJobInfo SMetroBusinessHour jobData = AnyJobInfo <$> restoreJobInfo SMetroBusinessHour jobData
  restoreAnyJobInfo SNyRegularMaster jobData = AnyJobInfo <$> restoreJobInfo SNyRegularMaster jobData
  restoreAnyJobInfo SNyRegularInstance jobData = AnyJobInfo <$> restoreJobInfo SNyRegularInstance jobData
  restoreAnyJobInfo SCrisRecon jobData = AnyJobInfo <$> restoreJobInfo SCrisRecon jobData
  restoreAnyJobInfo SPaymentOrderStatusCheck jobData = AnyJobInfo <$> restoreJobInfo SPaymentOrderStatusCheck jobData
  restoreAnyJobInfo SPartnerInvoiceDataExport jobData = AnyJobInfo <$> restoreJobInfo SPartnerInvoiceDataExport jobData

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

data CheckPNAndSendSMSJobData = CheckPNAndSendSMSJobData
  { bookingId :: Id Booking,
    emergencyContactPersonId :: Id Person,
    emergencyContactNumber :: Text,
    riderName :: Maybe Text,
    merchantOpCityId :: Id DMOC.MerchantOperatingCity,
    merchantId :: Id DM.Merchant,
    trackLink :: Text,
    redisKey :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckPNAndSendSMS

type instance JobContent 'CheckPNAndSendSMS = CheckPNAndSendSMSJobData

data ScheduledRideNotificationsToRiderJobData = ScheduledRideNotificationsToRiderJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    timeDiffEvent :: DRN.TimeDiffEvent,
    bookingStatus :: DEB.BookingStatus,
    notificationType :: DRN.NotificationType,
    notificationKey :: Text,
    bookingId :: Id Booking,
    personId :: Id Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledRideNotificationsToRider

type instance JobContent 'ScheduledRideNotificationsToRider = ScheduledRideNotificationsToRiderJobData

data ScheduleTagActionNotificationJobData = ScheduleTagActionNotificationJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    notificationType :: DRN.NotificationType,
    notificationKey :: Text,
    personId :: Id Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduleTagActionNotification

type instance JobContent 'ScheduleTagActionNotification = ScheduleTagActionNotificationJobData

data CheckExotelCallStatusAndNotifyBPPJobData = CheckExotelCallStatusAndNotifyBPPJobData
  { rideId :: Id DR.Ride,
    bppRideId :: Id DR.BPPRide,
    merchantId :: Id DM.Merchant,
    merchantOpCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckExotelCallStatusAndNotifyBPP

type instance JobContent 'CheckExotelCallStatusAndNotifyBPP = CheckExotelCallStatusAndNotifyBPPJobData

data UpdateCrisUtsDataJobData = UpdateCrisUtsDataJobData
  { integratedBPPConfigId :: Id DIntegratedBPPConfig.IntegratedBPPConfig
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UpdateCrisUtsData

type instance JobContent 'UpdateCrisUtsData = UpdateCrisUtsDataJobData

data CheckMultimodalConfirmFailJobData = CheckMultimodalConfirmFailJobData
  { bookingId :: Id DFTB.FRFSTicketBooking
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckMultimodalConfirmFail

type instance JobContent 'CheckMultimodalConfirmFail = CheckMultimodalConfirmFailJobData

data CheckRefundStatusJobData = CheckRefundStatusJobData
  { refundId :: Text,
    numberOfRetries :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckRefundStatus

type instance JobContent 'CheckRefundStatus = CheckRefundStatusJobData

data OtherJobTypesJobData = OtherJobTypesJobData
  { bookingId :: Id Booking,
    emergencyContactPersonId :: Id Person,
    emergencyContactNumber :: Text,
    riderName :: Maybe Text,
    merchantOpCityId :: Id DMOC.MerchantOperatingCity,
    merchantId :: Id DM.Merchant,
    trackLink :: Text,
    redisKey :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'OtherJobTypes

type instance JobContent 'OtherJobTypes = OtherJobTypesJobData

data SafetyIVRJobData = SafetyIVRJobData
  { rideId :: Id DRide.Ride,
    personId :: Id Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SafetyIVR

type instance JobContent 'SafetyIVR = SafetyIVRJobData

data CallPoliceApiJobData = CallPoliceApiJobData
  { rideId :: Id DRide.Ride,
    personId :: Id Person,
    jmCode :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CallPoliceApi

type instance JobContent 'CallPoliceApi = CallPoliceApiJobData

data SafetyCSAlertJobData = SafetyCSAlertJobData
  { rideId :: Id DRide.Ride,
    personId :: Id Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SafetyCSAlert

type instance JobContent 'SafetyCSAlert = SafetyCSAlertJobData

data MetroIncentivePayoutJobData = MetroIncentivePayoutJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    toScheduleNextPayout :: Bool,
    statusForRetry :: DFTB.CashbackStatus,
    schedulePayoutForDay :: Maybe Integer
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'MetroIncentivePayout

type instance JobContent 'MetroIncentivePayout = MetroIncentivePayoutJobData

newtype ScheduledRidePopupToRiderJobData = ScheduledRidePopupToRiderJobData
  { bookingId :: Id Booking
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledRidePopupToRider

type instance JobContent 'ScheduledRidePopupToRider = ScheduledRidePopupToRiderJobData

data ExecutePaymentIntentJobData = ExecutePaymentIntentJobData
  { rideId :: Id DRide.Ride,
    personId :: Id Person,
    fare :: Price,
    applicationFeeAmount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance JobInfoProcessor 'ExecutePaymentIntent

type instance JobContent 'ExecutePaymentIntent = ExecutePaymentIntentJobData

data CancelExecutePaymentIntentJobData = CancelExecutePaymentIntentJobData
  { bookingId :: Id Booking,
    personId :: Id Person,
    cancellationAmount :: PriceAPIEntity,
    rideId :: Id DRide.Ride
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance JobInfoProcessor 'CancelExecutePaymentIntent

type instance JobContent 'CancelExecutePaymentIntent = CancelExecutePaymentIntentJobData

data PostRideSafetyNotificationJobData = PostRideSafetyNotificationJobData
  { rideId :: Id DRide.Ride,
    personId :: Id Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'PostRideSafetyNotification

type instance JobContent 'PostRideSafetyNotification = PostRideSafetyNotificationJobData

data MetroBusinessHourJobData = MetroBusinessHourJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'MetroBusinessHour

type instance JobContent 'MetroBusinessHour = MetroBusinessHourJobData

data NyRegularInstanceJobData = NyRegularInstanceJobData
  { nyRegularSubscriptionId :: Id NyRegularSubscription.NyRegularSubscription,
    userId :: Id Person,
    scheduledTime :: UTCTime,
    expectedSchedulingHash :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'NyRegularInstance

type instance JobContent 'NyRegularInstance = NyRegularInstanceJobData

instance JobInfoProcessor 'NyRegularMaster

type instance JobContent 'NyRegularMaster = ()

data CrisReconJobData = CrisReconJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    checkSince :: Int, -- in hours
    scheduleItself :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CrisRecon

type instance JobContent 'CrisRecon = CrisReconJobData

data PaymentOrderStatusCheckJobData = PaymentOrderStatusCheckJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'PaymentOrderStatusCheck

type instance JobContent 'PaymentOrderStatusCheck = PaymentOrderStatusCheckJobData

data PartnerInvoiceDataExportJobData = PartnerInvoiceDataExportJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    scheduleItself :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'PartnerInvoiceDataExport

type instance JobContent 'PartnerInvoiceDataExport = PartnerInvoiceDataExportJobData
