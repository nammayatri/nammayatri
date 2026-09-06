{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DashboardAlert.Domain.Action.InternalAuth
  ( ResolveTopicReq (..),
    FleetOwnerLookup,
    fleetOwnerAccessTypes,
    resolveTopic,
  )
where

import DashboardAlert.Domain.Types.Audience
import DashboardAlert.Topic
import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common

data ResolveTopicReq = ResolveTopicReq
  { personId :: Text,
    accessType :: Maybe DashboardAccessType
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type FleetOwnerLookup m = Text -> m [Text]

fleetOwnerAccessTypes :: [DashboardAccessType]
fleetOwnerAccessTypes = [FLEET_OWNER, RENTAL_FLEET_OWNER]

resolveTopic :: MonadFlow m => FleetOwnerLookup m -> ResolveTopicReq -> m Topic
resolveTopic lookupFleetOwners req
  | maybe False (`elem` fleetOwnerAccessTypes) req.accessType = resolveFleetOwnerTopic
  | otherwise = pure accessTypeFallbackTopic
  where
    accessTypeFallbackTopic = accessTypeTopic (fromMaybe DASHBOARD_USER req.accessType)

    resolveFleetOwnerTopic = do
      fleetOwnerIds <- try @_ @SomeException (lookupFleetOwners req.personId)
      pure $ case fleetOwnerIds of
        Right (fleetOwnerId : _) -> fleetOwnerTopic (Id fleetOwnerId)
        _ -> accessTypeFallbackTopic
