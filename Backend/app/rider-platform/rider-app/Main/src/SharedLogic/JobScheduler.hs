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
{-# LANGUAGE TemplateHaskell #-}

module SharedLogic.JobScheduler where

import Data.Singletons.TH
import Domain.Types.Booking
import qualified Domain.Types.Extra.Booking as DEB
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler

data RiderJobType
  = CheckPNAndSendSMS
  | ScheduledRideNotificationsToRider
  | SafetyIVR
  | CallPoliceApi
  | CheckExotelCallStatusAndNotifyBPP
  | SafetyCSAlert
  | OtherJobTypes
  | MetroIncentivePayout
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''RiderJobType]
showSingInstance ''RiderJobType

instance JobProcessor RiderJobType where
  restoreAnyJobInfo :: Sing (e :: RiderJobType) -> Text -> Maybe (AnyJobInfo RiderJobType)
  restoreAnyJobInfo SCheckPNAndSendSMS jobData = AnyJobInfo <$> restoreJobInfo SCheckPNAndSendSMS jobData
  restoreAnyJobInfo SScheduledRideNotificationsToRider jobData = AnyJobInfo <$> restoreJobInfo SScheduledRideNotificationsToRider jobData
  restoreAnyJobInfo SSafetyIVR jobData = AnyJobInfo <$> restoreJobInfo SSafetyIVR jobData
  restoreAnyJobInfo SCallPoliceApi jobData = AnyJobInfo <$> restoreJobInfo SCallPoliceApi jobData
  restoreAnyJobInfo SSafetyCSAlert jobData = AnyJobInfo <$> restoreJobInfo SSafetyCSAlert jobData
  restoreAnyJobInfo SCheckExotelCallStatusAndNotifyBPP jobData = AnyJobInfo <$> restoreJobInfo SCheckExotelCallStatusAndNotifyBPP jobData
  restoreAnyJobInfo SOtherJobTypes jobData = AnyJobInfo <$> restoreJobInfo SOtherJobTypes jobData
  restoreAnyJobInfo SMetroIncentivePayout jobData = AnyJobInfo <$> restoreJobInfo SMetroIncentivePayout jobData

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

data CheckExotelCallStatusAndNotifyBPPJobData = CheckExotelCallStatusAndNotifyBPPJobData
  { rideId :: Id DR.Ride,
    bppRideId :: Id DR.BPPRide,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckExotelCallStatusAndNotifyBPP

type instance JobContent 'CheckExotelCallStatusAndNotifyBPP = CheckExotelCallStatusAndNotifyBPPJobData

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
