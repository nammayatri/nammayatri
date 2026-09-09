{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DashboardAlert.Domain.Types.Audience where

import DashboardAlert.Domain.Types.Common (Person)
import Data.Aeson
import qualified Data.UUID as UU
import Kernel.Prelude
import Kernel.Types.Id (Id (..))

data AlertPlatform
  = DriverPlatform
  | RiderPlatform
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

newtype Topic = Topic {getTopic :: Text}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data DashboardAccessType
  = DASHBOARD_USER
  | DASHBOARD_ADMIN
  | FLEET_OWNER
  | DASHBOARD_RELEASE_ADMIN
  | MERCHANT_ADMIN
  | RENTAL_FLEET_OWNER
  | MERCHANT_MAKER
  | MERCHANT_SERVER
  | DASHBOARD_OPERATOR
  | TICKET_DASHBOARD_USER
  | TICKET_DASHBOARD_MERCHANT
  | TICKET_DASHBOARD_ADMIN
  | TICKET_DASHBOARD_APPROVER
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema)

data Audience
  = AccessTypeAudience DashboardAccessType
  | FleetOwnerAudience (Id Person)
  deriving (Show, Eq, Generic)

isValidTopic :: Topic -> Bool
isValidTopic (Topic value) = isJust (UU.fromText value)
