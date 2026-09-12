{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Read-only view of the operator-action audit trail, without the app-typed
-- endpoint enum.
--
-- @Domain.Types.Transaction.Endpoint@ is a sum over the API action types of both
-- application packages, which pins it to lib-dashboard-api. The column itself is
-- @character varying@ holding that enum's Show form -- "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LIST"
-- -- so this view carries it as 'Text'.
--
-- The listing still serialises that field exactly as the typed enum's generic
-- JSON did ('TransactionEndpoint'), so the response is unchanged for every
-- consumer.
module Domain.Types.TransactionView
  ( TransactionRow (..),
    TransactionAPIEntity (..),
    RequestorAPIEntity (..),
    ListTransactionRes (..),
    TransactionEndpoint (..),
    mkTransactionAPIEntity,
  )
where

import Control.Applicative ((<|>))
import Dashboard.Common (Summary)
import qualified Dashboard.Common.Driver as Common
import qualified Dashboard.Common.Exotel as Common
import qualified Dashboard.Common.SpecialZone as Common
import qualified Dashboard.SafetyPlatform as Safety
import Data.Aeson (Value (..), object, (.=))
import Data.OpenApi (NamedSchema (..), ToSchema (..))
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id

-- | One transaction row, endpoint left as the text the database holds.
data TransactionRow = TransactionRow
  { id :: Text,
    requestorId :: Maybe Text,
    merchantId :: Maybe Text,
    endpoint :: Text,
    commonDriverId :: Maybe Text,
    commonRideId :: Maybe Text,
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }

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

data TransactionAPIEntity = TransactionAPIEntity
  { id :: Id TransactionRow,
    requestor :: RequestorAPIEntity,
    merchantId :: Maybe (Id DM.Merchant),
    endpoint :: TransactionEndpoint,
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, ToSchema)

data ListTransactionRes = ListTransactionRes
  { list :: [TransactionAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, ToSchema)

-- | The stored endpoint text, serialised the way lib-dashboard-api's typed
-- @Endpoint@ was: aeson's generic encoding of that sum type. Rebuilt from the
-- text so this module needs none of the app-owned action types.
--
-- "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LIST" was
-- @{"tag":"ProviderManagementAPI","contents":{"tag":"DRIVER","contents":"GET_DRIVER_LIST"}}@;
-- nullary endpoints were @{"tag":"DashboardUserLogin"}@. Anything the typed enum
-- could not represent comes out as its UnknownEndpoint.
newtype TransactionEndpoint = TransactionEndpoint Text
  deriving stock (Show, Generic)

instance ToJSON TransactionEndpoint where
  toJSON (TransactionEndpoint stored) =
    fromMaybe (tagged "UnknownEndpoint" Nothing) $
      (flip tagged Nothing <$> lookup stored nullaryEndpoints)
        <|> asum [(\payload -> tagged tag (Just payload)) <$> (T.stripPrefix prefix stored >>= parse) | (prefix, tag, parse) <- legacyPayloads]
        <|> platformAction stored
    where
      tagged :: Text -> Maybe Value -> Value
      tagged tag mbContents = object $ ("tag" .= tag) : maybe [] (\contents -> ["contents" .= contents]) mbContents

instance ToSchema TransactionEndpoint where
  declareNamedSchema _ = pure $ NamedSchema (Just "TransactionEndpoint") mempty

nullaryEndpoints :: [(Text, Text)]
nullaryEndpoints =
  [ ("DASHBOARD_USER/LOGIN", "DashboardUserLogin"),
    ("DASHBOARD_USER/LOGOUT", "DashboardUserLogout"),
    ("DASHBOARD_USER/TWO_FACTOR_ADMIN_RESET", "DashboardTwoFactorAdminReset"),
    ("DASHBOARD_USER/DELETE", "DashboardUserDelete"),
    ("DASHBOARD_USER/PASSWORD_RESET_BY_ADMIN", "DashboardUserPasswordResetByAdmin"),
    ("DASHBOARD_USER/EMAIL_CHANGE_BY_ADMIN", "DashboardUserEmailChangeByAdmin"),
    ("DASHBOARD_USER/MOBILE_CHANGE_BY_ADMIN", "DashboardUserMobileChangeByAdmin"),
    ("DASHBOARD_USER/ROLE_ASSIGN", "DashboardUserRoleAssign"),
    ("UNKNOWN_ENDPOINT", "UnknownEndpoint")
  ]

-- | Constructors whose payload is shown with the derived Show, re-encoded with
-- the payload type's own ToJSON.
legacyPayloads :: [(Text, Text, Text -> Maybe Value)]
legacyPayloads =
  [ ("DriverAPI ", "DriverAPI", \s -> toJSON <$> (readMaybe (T.unpack s) :: Maybe Common.DriverEndpoint)),
    ("ExotelAPI ", "ExotelAPI", \s -> toJSON <$> (readMaybe (T.unpack s) :: Maybe Common.ExotelEndpoint)),
    ("SpecialZoneAPI ", "SpecialZoneAPI", \s -> toJSON <$> (readMaybe (T.unpack s) :: Maybe Common.SpecialZoneEndpoint)),
    ("SafetyAPI ", "SafetyAPI", \s -> toJSON <$> (readMaybe (T.unpack s) :: Maybe Safety.SafetyEndpoint))
  ]

-- | PLATFORM/RESOURCE/ACTION. Each platform's action union is a sum of
-- resources over all-nullary action enums, so the resource is a tagged object
-- around the action string -- except provider issue management, a
-- single-constructor newtype, which aeson encodes as its contents alone.
platformAction :: Text -> Maybe Value
platformAction stored = do
  let (platform, rest) = T.breakOn "/" stored
  (tag, singleResource) <- lookup platform platforms
  let (resource, action) = T.breakOn "/" (T.drop 1 rest)
      actionName = T.drop 1 action
  guard $ not (T.null resource || T.null actionName || "/" `T.isInfixOf` actionName)
  let contents
        | singleResource = String actionName
        | otherwise = object ["tag" .= resource, "contents" .= actionName]
  pure $ object ["tag" .= tag, "contents" .= contents]
  where
    platforms :: [(Text, (Text, Bool))]
    platforms =
      [ ("RIDER_MANAGEMENT", ("RiderManagementAPI", False)),
        ("RIDER_APP_MANAGEMENT", ("RiderAppManagementAPI", False)),
        ("RIDER_ISSUE_MANAGEMENT", ("RiderIssueManagementAPI", False)),
        ("RIDER_RIDE_BOOKING", ("RiderRideBookingAPI", False)),
        ("PROVIDER_FLEET", ("ProviderFleetAPI", False)),
        ("PROVIDER_OPERATOR", ("ProviderOperatorAPI", False)),
        ("PROVIDER_MANAGEMENT", ("ProviderManagementAPI", False)),
        ("PROVIDER_APP_MANAGEMENT", ("ProviderAppManagementAPI", False)),
        ("PROVIDER_ISSUE_MANAGEMENT", ("ProviderIssueManagementAPI", True)),
        ("PROVIDER_RIDE_BOOKING", ("ProviderRideBookingAPI", False))
      ]

mkTransactionAPIEntity :: TransactionRow -> DP.DecryptedPerson -> TransactionAPIEntity
mkTransactionAPIEntity row requestor =
  TransactionAPIEntity
    { id = Id row.id,
      requestor = mkRequestorAPIEntity requestor,
      merchantId = Id <$> row.merchantId,
      endpoint = TransactionEndpoint row.endpoint,
      commonDriverId = Id <$> row.commonDriverId,
      commonRideId = Id <$> row.commonRideId,
      request = row.request,
      response = row.response,
      responseError = row.responseError,
      createdAt = row.createdAt
    }
  where
    mkRequestorAPIEntity DP.Person {..} = RequestorAPIEntity {registeredAt = createdAt, ..}
