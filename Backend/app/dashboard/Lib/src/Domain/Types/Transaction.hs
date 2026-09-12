{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The dashboard audit trail, parameterised by the action type of whichever
-- package owns the endpoint.
--
-- The ten per-platform constructors this type used to carry were an exact
-- duplicate of the action union -- their rendered form was identical -- so they
-- collapse into a single 'ActionAPI'. Every rendered string is unchanged, which
-- matters because the @transaction.endpoint@ column stores @show@ output. Rows
-- are only written through this type; they are read back as text
-- ('Domain.Types.TransactionView'), so there is no Read instance.
module Domain.Types.Transaction where

import qualified Dashboard.Common.Booking as Common
import qualified Dashboard.Common.Exotel as Common
import qualified Dashboard.Common.SpecialZone as Common
import qualified Dashboard.SafetyPlatform as Safety
import Data.OpenApi hiding (email, name)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import Kernel.Types.Id
import qualified Text.Show

data Transaction uat = Transaction
  { id :: Id (Transaction uat),
    requestorId :: Maybe (Id DP.Person),
    serverName :: Maybe DSN.ServerName,
    merchantId :: Maybe (Id DM.Merchant), -- will be Nothing for admin apis
    endpoint :: Endpoint uat,
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }

data Endpoint uat
  = -- | Any action owned by a server's own action union. Renders exactly as the
    -- action does, so the ten former per-platform constructors are indistinguishable
    -- from this one on disk.
    ActionAPI uat
  | ExotelAPI Common.ExotelEndpoint
  | SpecialZoneAPI Common.SpecialZoneEndpoint
  | SafetyAPI Safety.SafetyEndpoint
  | DashboardUserLogin
  | DashboardUserLogout
  | DashboardTwoFactorAdminReset
  | DashboardUserDelete
  | DashboardUserPasswordResetByAdmin
  | DashboardUserEmailChangeByAdmin
  | DashboardUserMobileChangeByAdmin
  | DashboardUserRoleAssign
  | UnknownEndpoint
  deriving (Eq, Ord, Generic)

-- Explicit contexts: anyclass deriving cannot pick a @GToSchema@ instance for a
-- bare type variable, so the constraint has to be stated.
instance ToJSON uat => ToJSON (Endpoint uat)

instance FromJSON uat => FromJSON (Endpoint uat)

instance ToSchema uat => ToSchema (Endpoint uat) where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance Show uat => Text.Show.Show (Endpoint uat) where
  show = \case
    ActionAPI e -> show e
    ExotelAPI e -> "ExotelAPI " <> show e
    SpecialZoneAPI e -> "SpecialZoneAPI " <> show e
    SafetyAPI e -> "SafetyAPI " <> show e
    DashboardUserLogin -> "DASHBOARD_USER/LOGIN"
    DashboardUserLogout -> "DASHBOARD_USER/LOGOUT"
    DashboardTwoFactorAdminReset -> "DASHBOARD_USER/TWO_FACTOR_ADMIN_RESET"
    DashboardUserDelete -> "DASHBOARD_USER/DELETE"
    DashboardUserPasswordResetByAdmin -> "DASHBOARD_USER/PASSWORD_RESET_BY_ADMIN"
    DashboardUserEmailChangeByAdmin -> "DASHBOARD_USER/EMAIL_CHANGE_BY_ADMIN"
    DashboardUserMobileChangeByAdmin -> "DASHBOARD_USER/MOBILE_CHANGE_BY_ADMIN"
    DashboardUserRoleAssign -> "DASHBOARD_USER/ROLE_ASSIGN"
    UnknownEndpoint -> "UNKNOWN_ENDPOINT"
