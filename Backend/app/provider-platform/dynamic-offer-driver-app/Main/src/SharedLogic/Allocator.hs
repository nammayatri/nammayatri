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

module SharedLogic.Allocator where

import Data.Singletons.TH
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.Overlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler

data AllocatorJobType
  = SendSearchRequestToDriver
  | SendPaymentReminderToDriver
  | UnsubscribeDriverForPaymentOverdue
  | UnblockDriver
  | SendPDNNotificationToDriver
  | MandateExecution
  | CalculateDriverFees
  | OrderAndNotificationStatusUpdate
  | SendOverlay
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''AllocatorJobType]
showSingInstance ''AllocatorJobType

instance JobProcessor AllocatorJobType where
  restoreAnyJobInfo :: Sing (e :: AllocatorJobType) -> Text -> Maybe (AnyJobInfo AllocatorJobType)
  restoreAnyJobInfo SSendSearchRequestToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendSearchRequestToDriver jobData
  restoreAnyJobInfo SSendPaymentReminderToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendPaymentReminderToDriver jobData
  restoreAnyJobInfo SUnsubscribeDriverForPaymentOverdue jobData = AnyJobInfo <$> restoreJobInfo SUnsubscribeDriverForPaymentOverdue jobData
  restoreAnyJobInfo SUnblockDriver jobData = AnyJobInfo <$> restoreJobInfo SUnblockDriver jobData
  restoreAnyJobInfo SSendPDNNotificationToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendPDNNotificationToDriver jobData
  restoreAnyJobInfo SMandateExecution jobData = AnyJobInfo <$> restoreJobInfo SMandateExecution jobData
  restoreAnyJobInfo SCalculateDriverFees jobData = AnyJobInfo <$> restoreJobInfo SCalculateDriverFees jobData
  restoreAnyJobInfo SOrderAndNotificationStatusUpdate jobData = AnyJobInfo <$> restoreJobInfo SOrderAndNotificationStatusUpdate jobData
  restoreAnyJobInfo SSendOverlay jobData = AnyJobInfo <$> restoreJobInfo SSendOverlay jobData

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { searchTryId :: Id DST.SearchTry,
    estimatedRideDistance :: Meters,
    driverExtraFeeBounds :: Maybe DFP.DriverExtraFeeBounds
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendSearchRequestToDriver

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

data SendPaymentReminderToDriverJobData = SendPaymentReminderToDriverJobData
  { startTime :: UTCTime,
    endTime :: UTCTime,
    timeDiff :: Seconds,
    merchantId :: Maybe (Id DM.Merchant)
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendPaymentReminderToDriver

type instance JobContent 'SendPaymentReminderToDriver = SendPaymentReminderToDriverJobData

data UnsubscribeDriverForPaymentOverdueJobData = UnsubscribeDriverForPaymentOverdueJobData
  { startTime :: UTCTime,
    timeDiff :: Seconds,
    merchantId :: Maybe (Id DM.Merchant)
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnsubscribeDriverForPaymentOverdue

type instance JobContent 'UnsubscribeDriverForPaymentOverdue = UnsubscribeDriverForPaymentOverdueJobData

newtype UnblockDriverRequestJobData = UnblockDriverRequestJobData
  { driverId :: Id DP.Driver
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnblockDriver

type instance JobContent 'UnblockDriver = UnblockDriverRequestJobData

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

data SendPDNNotificationToDriverJobData = SendPDNNotificationToDriverJobData
  { startTime :: UTCTime,
    endTime :: UTCTime,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendPDNNotificationToDriver

type instance JobContent 'SendPDNNotificationToDriver = SendPDNNotificationToDriverJobData

data MandateExecutionInfo = MandateExecutionInfo
  { startTime :: UTCTime,
    endTime :: UTCTime,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'MandateExecution

type instance JobContent 'MandateExecution = MandateExecutionInfo

data CalculateDriverFeesJobData = CalculateDriverFeesJobData
  { merchantId :: Id DM.Merchant,
    startTime :: UTCTime,
    endTime :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CalculateDriverFees

type instance JobContent 'CalculateDriverFees = CalculateDriverFeesJobData

newtype OrderAndNotificationStatusUpdateJobData = OrderAndNotificationStatusUpdateJobData
  { merchantId :: Id DM.Merchant
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
    overlayBatchSize :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendOverlay

type instance JobContent 'SendOverlay = SendOverlayJobData
