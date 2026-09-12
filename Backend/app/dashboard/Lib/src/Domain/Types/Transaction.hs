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
-- matters because the @transaction.endpoint@ column stores @show@ output and
-- existing rows must still parse.
module Domain.Types.Transaction where

import Control.Lens.Operators
import qualified Dashboard.Common.Booking as Common
import qualified Dashboard.Common.Driver as Common
import qualified Dashboard.Common.Exotel as Common
import qualified Dashboard.Common.SpecialZone as Common
import qualified Dashboard.SafetyPlatform as Safety
import qualified Data.List as List
import Data.OpenApi hiding (email, name)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import Kernel.Types.Id
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import qualified Text.Read
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

data TransactionAPIEntity uat = TransactionAPIEntity
  { id :: Id (Transaction uat),
    requestor :: RequestorAPIEntity,
    merchantId :: Maybe (Id DM.Merchant),
    endpoint :: Endpoint uat, -- search by this also
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON uat => ToJSON (TransactionAPIEntity uat)

instance FromJSON uat => FromJSON (TransactionAPIEntity uat)

instance ToSchema uat => ToSchema (TransactionAPIEntity uat) where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

data RequestorAPIEntity = RequestorAPIEntity
  { id :: Id DP.Person,
    firstName :: Text,
    lastName :: Text,
    email :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    registeredAt :: UTCTime,
    verified :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Endpoint uat
  = -- | Any action owned by a server's own action union. Renders exactly as the
    -- action does, so the ten former per-platform constructors are indistinguishable
    -- from this one on disk.
    ActionAPI uat
  | DriverAPI Common.DriverEndpoint
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
    DriverAPI e -> "DriverAPI " <> show e
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

instance Read uat => Text.Read.Read (Endpoint uat) where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(DriverAPI v1, r2) | r1 <- stripPrefix "DriverAPI " r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [(ExotelAPI v1, r2) | r1 <- stripPrefix "ExotelAPI " r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [(SpecialZoneAPI v1, r2) | r1 <- stripPrefix "SpecialZoneAPI " r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [(SafetyAPI v1, r2) | r1 <- stripPrefix "SafetyAPI " r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [(DashboardTwoFactorAdminReset, r1) | r1 <- stripPrefix "DASHBOARD_USER/TWO_FACTOR_ADMIN_RESET" r]
            ++ [(DashboardUserPasswordResetByAdmin, r1) | r1 <- stripPrefix "DASHBOARD_USER/PASSWORD_RESET_BY_ADMIN" r]
            ++ [(DashboardUserEmailChangeByAdmin, r1) | r1 <- stripPrefix "DASHBOARD_USER/EMAIL_CHANGE_BY_ADMIN" r]
            ++ [(DashboardUserMobileChangeByAdmin, r1) | r1 <- stripPrefix "DASHBOARD_USER/MOBILE_CHANGE_BY_ADMIN" r]
            ++ [(DashboardUserRoleAssign, r1) | r1 <- stripPrefix "DASHBOARD_USER/ROLE_ASSIGN" r]
            ++ [(DashboardUserLogin, r1) | r1 <- stripPrefix "DASHBOARD_USER/LOGIN" r]
            ++ [(DashboardUserLogout, r1) | r1 <- stripPrefix "DASHBOARD_USER/LOGOUT" r]
            ++ [(DashboardUserDelete, r1) | r1 <- stripPrefix "DASHBOARD_USER/DELETE" r]
            ++ [(UnknownEndpoint, r1) | r1 <- stripPrefix "UNKNOWN_ENDPOINT" r]
            -- last: an action renders bare, so it must not shadow the prefixed forms above
            ++ [(ActionAPI v1, r2) | (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance Show uat => ToHttpApiData (Endpoint uat) where
  toUrlPiece = show

instance Read uat => FromHttpApiData (Endpoint uat) where
  parseUrlPiece = maybe (Left "parse Endpoint failed") Right . readMaybe . toString

instance ToParamSchema (Endpoint uat) where
  toParamSchema _ = mempty & type_ ?~ OpenApiString
