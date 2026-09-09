{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DashboardAlert.Topic
  ( accessTypeTopic,
    fleetOwnerTopic,
    audienceTopic,
    allAccessTypeTopics,
  )
where

import DashboardAlert.Domain.Types.Audience
import DashboardAlert.Domain.Types.Common (Person)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))

accessTypeTopic :: DashboardAccessType -> Topic
accessTypeTopic = \case
  DASHBOARD_USER -> Topic "da5b0a2d-0000-0000-0000-000000000001"
  DASHBOARD_ADMIN -> Topic "da5b0a2d-0000-0000-0000-000000000002"
  FLEET_OWNER -> Topic "da5b0a2d-0000-0000-0000-000000000003"
  DASHBOARD_RELEASE_ADMIN -> Topic "da5b0a2d-0000-0000-0000-000000000004"
  MERCHANT_ADMIN -> Topic "da5b0a2d-0000-0000-0000-000000000005"
  RENTAL_FLEET_OWNER -> Topic "da5b0a2d-0000-0000-0000-000000000006"
  MERCHANT_MAKER -> Topic "da5b0a2d-0000-0000-0000-000000000007"
  MERCHANT_SERVER -> Topic "da5b0a2d-0000-0000-0000-000000000008"
  DASHBOARD_OPERATOR -> Topic "da5b0a2d-0000-0000-0000-000000000009"
  TICKET_DASHBOARD_USER -> Topic "da5b0a2d-0000-0000-0000-00000000000a"
  TICKET_DASHBOARD_MERCHANT -> Topic "da5b0a2d-0000-0000-0000-00000000000b"
  TICKET_DASHBOARD_ADMIN -> Topic "da5b0a2d-0000-0000-0000-00000000000c"
  TICKET_DASHBOARD_APPROVER -> Topic "da5b0a2d-0000-0000-0000-00000000000d"

fleetOwnerTopic :: Id Person -> Topic
fleetOwnerTopic personId = Topic personId.getId

audienceTopic :: Audience -> Topic
audienceTopic = \case
  AccessTypeAudience accessType -> accessTypeTopic accessType
  FleetOwnerAudience personId -> fleetOwnerTopic personId

allAccessTypeTopics :: [Topic]
allAccessTypeTopics = map accessTypeTopic [minBound .. maxBound]
