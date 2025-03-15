{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Transaction where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement as ProviderAppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement as RiderAppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking as ProviderRideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking as RiderRideBooking
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet as ProviderFleet
import qualified "shared-services" API.Types.ProviderPlatform.IssueManagement as ProviderIssueManagement
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management as ProviderManagement
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Operator as ProviderOperator
import qualified "shared-services" API.Types.RiderPlatform.IssueManagement as RiderIssueManagement
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management as RiderManagement
import Control.Lens.Operators
import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified "dashboard-helper-api" Dashboard.Common.Driver as Common
import qualified "dashboard-helper-api" Dashboard.Common.Exotel as Common
import qualified "dashboard-helper-api" Dashboard.Common.SpecialZone as Common
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import qualified Data.List as List
import Data.OpenApi hiding (email, name)
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Id
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import qualified Text.Show (show)

-- request is raw Text here, because if some field will be changed, we can't parse it
data Transaction = Transaction
  { id :: Id Transaction,
    requestorId :: Maybe (Id DP.Person),
    serverName :: Maybe DSN.ServerName,
    merchantId :: Maybe (Id DM.Merchant), -- will be Nothing for admin apis
    endpoint :: Endpoint,
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }

data TransactionAPIEntity = TransactionAPIEntity
  { id :: Id Transaction,
    requestor :: RequestorAPIEntity,
    merchantId :: Maybe (Id DM.Merchant),
    endpoint :: Endpoint, -- search by this also
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

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

-- TODO to be deprecated after move all apis to DSL
data Endpoint
  = DriverAPI Common.DriverEndpoint
  | ExotelAPI Common.ExotelEndpoint
  | SpecialZoneAPI Common.SpecialZoneEndpoint
  | SafetyAPI Safety.SafetyEndpoint
  | RiderManagementAPI RiderManagement.ManagementUserActionType
  | RiderAppManagementAPI RiderAppManagement.AppManagementUserActionType
  | RiderIssueManagementAPI RiderIssueManagement.IssueManagementUserActionType
  | RiderRideBookingAPI RiderRideBooking.RideBookingUserActionType
  | ProviderFleetAPI ProviderFleet.FleetUserActionType
  | ProviderOperatorAPI ProviderOperator.OperatorUserActionType
  | ProviderManagementAPI ProviderManagement.ManagementUserActionType
  | ProviderAppManagementAPI ProviderAppManagement.AppManagementUserActionType
  | ProviderIssueManagementAPI ProviderIssueManagement.IssueManagementUserActionType
  | ProviderRideBookingAPI ProviderRideBooking.RideBookingUserActionType
  | UnknownEndpoint -- Just to avoid unnecessary error throwing
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance Show Endpoint where
  show = \case
    DriverAPI e -> "DriverAPI " <> show e
    ExotelAPI e -> "ExotelAPI " <> show e
    SpecialZoneAPI e -> "SpecialZoneAPI " <> show e
    SafetyAPI e -> "SafetyAPI " <> show e
    RiderManagementAPI e -> "RIDER_MANAGEMENT/" <> show e
    RiderAppManagementAPI e -> "RIDER_APP_MANAGEMENT/" <> show e
    RiderIssueManagementAPI e -> "RIDER_ISSUE_MANAGEMENT/" <> show e
    RiderRideBookingAPI e -> "RIDER_RIDE_BOOKING/" <> show e
    ProviderFleetAPI e -> "PROVIDER_FLEET/" <> show e
    ProviderOperatorAPI e -> "PROVIDER_OPERATOR/" <> show e
    ProviderManagementAPI e -> "PROVIDER_MANAGEMENT/" <> show e
    ProviderAppManagementAPI e -> "PROVIDER_APP_MANAGEMENT/" <> show e
    ProviderIssueManagementAPI e -> "PROVIDER_ISSUE_MANAGEMENT/" <> show e
    ProviderRideBookingAPI e -> "PROVIDER_RIDE_BOOKING/" <> show e
    UnknownEndpoint -> "UNKNOWN_ENDPOINT"

$(mkBeamInstancesForEnum ''Endpoint)

instance FromHttpApiData Endpoint where
  parseQueryParam = readEither

instance ToHttpApiData Endpoint where
  toUrlPiece = show

instance ToParamSchema Endpoint where
  toParamSchema _ =
    mempty
      & title ?~ "Endpoint"
      & type_ ?~ OpenApiString
      & format ?~ "PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK,.."

instance Read Endpoint where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (DriverAPI v1, r2)
            | r1 <- stripPrefix "DriverAPI " r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (ExotelAPI v1, r2)
                 | r1 <- stripPrefix "ExotelAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (SpecialZoneAPI v1, r2)
                 | r1 <- stripPrefix "SpecialZoneAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (SafetyAPI v1, r2)
                 | r1 <- stripPrefix "SafetyAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RiderManagementAPI v1, r2)
                 | r1 <- stripPrefix "RIDER_MANAGEMENT/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RiderAppManagementAPI v1, r2)
                 | r1 <- stripPrefix "RIDER_APP_MANAGEMENT/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RiderIssueManagementAPI v1, r2)
                 | r1 <- stripPrefix "RIDER_ISSUE_MANAGEMENT/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RiderRideBookingAPI v1, r2)
                 | r1 <- stripPrefix "RIDER_RIDE_BOOKING/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderFleetAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_FLEET/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderOperatorAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_OPERATOR/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderManagementAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_MANAGEMENT/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderAppManagementAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_APP_MANAGEMENT/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderIssueManagementAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_ISSUE_MANAGEMENT/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderRideBookingAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_RIDE_BOOKING/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (UnknownEndpoint, r1)
                 | r1 <- stripPrefix "UNKNOWN_ENDPOINT" r
               ]
      )
    where
      app_prec = 9
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

-- Should be used only for autogenerated APIs
castEndpoint :: DMatrix.UserActionType -> Endpoint
castEndpoint (DMatrix.PROVIDER_FLEET uat) = ProviderFleetAPI uat
castEndpoint (DMatrix.PROVIDER_OPERATOR uat) = ProviderOperatorAPI uat
castEndpoint (DMatrix.PROVIDER_MANAGEMENT uat) = ProviderManagementAPI uat
castEndpoint (DMatrix.PROVIDER_APP_MANAGEMENT uat) = ProviderAppManagementAPI uat
castEndpoint (DMatrix.PROVIDER_ISSUE_MANAGEMENT uat) = ProviderIssueManagementAPI uat
castEndpoint (DMatrix.PROVIDER_RIDE_BOOKING uat) = ProviderRideBookingAPI uat
castEndpoint (DMatrix.RIDER_MANAGEMENT uat) = RiderManagementAPI uat
castEndpoint (DMatrix.RIDER_APP_MANAGEMENT uat) = RiderAppManagementAPI uat
castEndpoint (DMatrix.RIDER_ISSUE_MANAGEMENT uat) = RiderIssueManagementAPI uat
castEndpoint (DMatrix.RIDER_RIDE_BOOKING uat) = RiderRideBookingAPI uat
castEndpoint _ = UnknownEndpoint -- should not appear
