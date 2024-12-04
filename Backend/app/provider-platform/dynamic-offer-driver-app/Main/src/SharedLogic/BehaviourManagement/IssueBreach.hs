{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software :: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https :://www.gnu.org/licenses/>.
-}
module SharedLogic.BehaviourManagement.IssueBreach where

import Data.Aeson
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Utils.TH
import Tools.Beam.UtilsTH

data IssueBreachType = EXTRA_FARE_MITIGATION deriving (Generic, Show, Eq, Read, ToSchema, ToParamSchema)

data IssueBreachBlockType = IBSoft | IBHard deriving (Generic, Show, Eq, Read, ToSchema, ToParamSchema)

-- soft is stier level block, driver will be able to go online
-- hard is driver level block, driver will not be able to go online

data IssueBreachConfig = IssueBreachConfig
  { ibCountWindowSizeInDays :: Int,
    ibDailyMinRidesforBlocking :: Int,
    ibWeeklyMinRidesforBlocking :: Int,
    ibRateThresholdDaily :: Int,
    ibRateThresholdWeekly :: Int,
    ibDailyOffenceSuspensionTimeInHours :: Int,
    ibWeeklyOffenceSuspensionTimeInHours :: Int,
    ibDailyCooldownTimeInHours :: Int,
    ibWeeklyCooldownTimeInHours :: Int,
    ibAllowedServiceTiers :: [ServiceTierType],
    ibIssueBreachType :: IssueBreachType,
    ibBlockType :: IssueBreachBlockType,
    ibNotifyInMins :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Read, ToSchema)

data IssueBreachCooldownTime = IssueBreachCooldownTime
  { ibDailyCooldownTimeInHours :: Maybe UTCTime,
    ibWeeklyCooldownTimeInHours :: Maybe UTCTime,
    ibType :: IssueBreachType
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Read, ToSchema)

instance ToJSON IssueBreachType where
  toJSON = String . show

instance FromJSON IssueBreachType where
  parseJSON = fmap read . parseJSON

instance ToJSON IssueBreachBlockType where
  toJSON = String . show

instance FromJSON IssueBreachBlockType where
  parseJSON = fmap read . parseJSON

$(mkHttpInstancesForEnum ''IssueBreachType)

$(mkBeamInstancesForEnumAndList ''IssueBreachType)
