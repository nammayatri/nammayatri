{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The dashboard actions this server owns. Each server owns the enum for its
-- own endpoints, so lib-dashboard never has to name any of them and neither
-- application server depends on the other.
--
-- The rendered endpoint id is unchanged from when this was one shared union --
-- @capability_endpoint@ rows must keep matching.
module Domain.Types.AccessMatrix (module Domain.Types.AccessMatrix, module Reexport) where

import qualified "this" API.Types.Dashboard.AppManagement as ProviderAppManagement
import qualified "this" API.Types.Dashboard.RideBooking as ProviderRideBooking
import qualified "this" API.Types.ProviderPlatform.Fleet as ProviderFleet
import qualified "shared-services" API.Types.ProviderPlatform.IssueManagement as ProviderIssueManagement
import qualified "this" API.Types.ProviderPlatform.Management as ProviderManagement
import qualified "this" API.Types.ProviderPlatform.Operator as ProviderOperator
import Data.Singletons.TH
import qualified Data.Text as T
import "lib-dashboard" Domain.Types.ServerName as Reexport (ServerName (..))
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Text.Show
import "lib-dashboard" Tools.Auth.ApiAuth as Reexport (ApiAccessLevel (..), ApiEntity (..), ApiTokenInfo (..), IsUserActionType (..), type (/))
import qualified "lib-dashboard" Tools.Auth.ApiAuth as Auth

data UserActionType
  = PROVIDER_FLEET ProviderFleet.FleetUserActionType
  | PROVIDER_OPERATOR ProviderOperator.OperatorUserActionType
  | PROVIDER_MANAGEMENT ProviderManagement.ManagementUserActionType
  | PROVIDER_APP_MANAGEMENT ProviderAppManagement.AppManagementUserActionType
  | PROVIDER_ISSUE_MANAGEMENT ProviderIssueManagement.IssueManagementUserActionType
  | PROVIDER_RIDE_BOOKING ProviderRideBooking.RideBookingUserActionType
  deriving (Read, Generic, ToSchema, Eq, Ord)

instance Text.Show.Show UserActionType where
  show = \case
    PROVIDER_FLEET uat -> "PROVIDER_FLEET/" <> show uat
    PROVIDER_OPERATOR uat -> "PROVIDER_OPERATOR/" <> show uat
    PROVIDER_MANAGEMENT uat -> "PROVIDER_MANAGEMENT/" <> show uat
    PROVIDER_APP_MANAGEMENT uat -> "PROVIDER_APP_MANAGEMENT/" <> show uat
    PROVIDER_ISSUE_MANAGEMENT uat -> "PROVIDER_ISSUE_MANAGEMENT/" <> show uat
    PROVIDER_RIDE_BOOKING uat -> "PROVIDER_RIDE_BOOKING/" <> show uat

instance Auth.IsUserActionType UserActionType where
  showUserActionType = T.pack . show

genSingletons [''UserActionType]

-- | This server's auth combinator, pinning the generic machinery to the union
-- above. Route definitions keep their existing three-argument shape.
type ApiAuth (sn :: DSN.ServerName) (ae :: Auth.ApiEntity) (uat :: k) = Auth.ApiAuthFor UserActionType sn ae uat
