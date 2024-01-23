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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler

data RiderJobType
  = CheckPNAndSendSMS
  | OtherJobTypes
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''RiderJobType]
showSingInstance ''RiderJobType

instance JobProcessor RiderJobType where
  restoreAnyJobInfo :: Sing (e :: RiderJobType) -> Text -> Maybe (AnyJobInfo RiderJobType)
  restoreAnyJobInfo SCheckPNAndSendSMS jobData = AnyJobInfo <$> restoreJobInfo SCheckPNAndSendSMS jobData
  restoreAnyJobInfo SOtherJobTypes jobData = AnyJobInfo <$> restoreJobInfo SOtherJobTypes jobData

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
