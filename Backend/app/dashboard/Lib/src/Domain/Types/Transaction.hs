{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Transaction where

import qualified "dynamic-offer-driver-app" API.Dashboard.Management.Overlay as BPP
import qualified "dynamic-offer-driver-app" API.Dashboard.Management.Subscription as BPP
import qualified "rider-app" API.Dashboard.RideBooking.Booking as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Cancel as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Confirm as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Frontend as BAP
import qualified "dynamic-offer-driver-app" API.Dashboard.RideBooking.Maps as BPP
import qualified "rider-app" API.Dashboard.RideBooking.Maps as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Profile as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Registration as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Search as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Select as BAP
import qualified "rider-app" API.Dashboard.Tickets as ADT
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet as ProviderFleet
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management as ProviderManagement
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.RideBooking as ProviderRideBooking
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management as RiderManagement
-- import qualified "dashboard-helper-api" API.Types.RiderPlatform.RideBooking as RiderRideBooking
import Control.Lens.Operators
import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified "dashboard-helper-api" Dashboard.Common.Driver as Common
import qualified "dashboard-helper-api" Dashboard.Common.Exotel as Common
import qualified "dashboard-helper-api" Dashboard.Common.SpecialZone as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Ride as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Customer as Common
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import qualified Data.List as List
import Data.OpenApi hiding (email, name)
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import qualified IssueManagement.Common.Dashboard.Issue as Common
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
  = RideAPI Common.RideEndpoint
  | DriverAPI Common.DriverEndpoint
  | DriverRegistrationAPI Common.DriverRegistrationEndpoint
  | CustomerAPI Common.CustomerEndpoint
  | ExotelAPI Common.ExotelEndpoint
  | IssueAPI Common.IssueEndpoint
  | VolunteerAPI Common.VolunteerEndpoint
  | RegistrationAPI BAP.RegistrationEndPoint
  | SearchAPI BAP.RideSearchEndPoint
  | SelectAPI BAP.RideEstimatesEndPoint
  | ConfirmAPI BAP.RideConfirmEndPoint
  | RBooking BAP.RideBookingEndPoint
  | ProfileAPI BAP.ProfileEndPoint
  | MapsAPI BAP.MapEndPoints
  | FlowStatusAPI BAP.RideNotifyEventEndPoint
  | CancelAPI BAP.RideCancelEndPoint
  | SpecialZoneAPI Common.SpecialZoneEndpoint
  | SubscriptionAPI BPP.SubscriptionEndpoint
  | OverlayAPI BPP.OverlayEndpoint
  | TicketsAPI ADT.TicketBookingEndpoint
  | MapAPI BPP.MapEndPoint
  | SafetyAPI Safety.SafetyEndpoint
  | RiderManagementAPI RiderManagement.ManagementUserActionType
  | -- | RiderRideBookingAPI RiderRideBooking.RideBookingUserActionType
    ProviderFleetAPI ProviderFleet.FleetUserActionType
  | ProviderManagementAPI ProviderManagement.ManagementUserActionType
  | ProviderRideBookingAPI ProviderRideBooking.RideBookingUserActionType
  | UnknownEndpoint -- Just to avoid unnecessary error throwing
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance Show Endpoint where
  show = \case
    RideAPI e -> "RideAPI " <> show e
    DriverAPI e -> "DriverAPI " <> show e
    DriverRegistrationAPI e -> "DriverRegistrationAPI " <> show e
    CustomerAPI e -> "CustomerAPI " <> show e
    ExotelAPI e -> "ExotelAPI " <> show e
    IssueAPI e -> "IssueAPI " <> show e
    VolunteerAPI e -> "VolunteerAPI " <> show e
    RegistrationAPI e -> "RegistrationAPI " <> show e
    SearchAPI e -> "SearchAPI " <> show e
    SelectAPI e -> "SelectAPI " <> show e
    ConfirmAPI e -> "ConfirmAPI " <> show e
    RBooking e -> "RBooking " <> show e
    ProfileAPI e -> "ProfileAPI " <> show e
    MapsAPI e -> "MapsAPI " <> show e
    FlowStatusAPI e -> "FlowStatusAPI " <> show e
    CancelAPI e -> "CancelAPI " <> show e
    SpecialZoneAPI e -> "SpecialZoneAPI " <> show e
    SubscriptionAPI e -> "SubscriptionAPI " <> show e
    OverlayAPI e -> "OverlayAPI " <> show e
    TicketsAPI e -> "TicketsAPI " <> show e
    MapAPI e -> "MapAPI " <> show e
    SafetyAPI e -> "SafetyAPI " <> show e
    RiderManagementAPI e -> "RIDER_MANAGEMENT/" <> show e
    -- RiderRideBookingAPI e -> "RIDER_RIDE_BOOKING/" <> show e
    ProviderFleetAPI e -> "PROVIDER_FLEET/" <> show e
    ProviderManagementAPI e -> "PROVIDER_MANAGEMENT/" <> show e
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
          [ (RideAPI v1, r2)
            | r1 <- stripPrefix "RideAPI " r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (DriverAPI v1, r2)
                 | r1 <- stripPrefix "DriverAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (DriverRegistrationAPI v1, r2)
                 | r1 <- stripPrefix "DriverRegistrationAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (CustomerAPI v1, r2)
                 | r1 <- stripPrefix "CustomerAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ExotelAPI v1, r2)
                 | r1 <- stripPrefix "ExotelAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (IssueAPI v1, r2)
                 | r1 <- stripPrefix "IssueAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (VolunteerAPI v1, r2)
                 | r1 <- stripPrefix "VolunteerAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RegistrationAPI v1, r2)
                 | r1 <- stripPrefix "RegistrationAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (SearchAPI v1, r2)
                 | r1 <- stripPrefix "SearchAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (SelectAPI v1, r2)
                 | r1 <- stripPrefix "SelectAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ConfirmAPI v1, r2)
                 | r1 <- stripPrefix "ConfirmAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RBooking v1, r2)
                 | r1 <- stripPrefix "RBooking " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProfileAPI v1, r2)
                 | r1 <- stripPrefix "ProfileAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MapsAPI v1, r2)
                 | r1 <- stripPrefix "MapsAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (FlowStatusAPI v1, r2)
                 | r1 <- stripPrefix "FlowStatusAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (CancelAPI v1, r2)
                 | r1 <- stripPrefix "CancelAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (SpecialZoneAPI v1, r2)
                 | r1 <- stripPrefix "SpecialZoneAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (SubscriptionAPI v1, r2)
                 | r1 <- stripPrefix "SubscriptionAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (OverlayAPI v1, r2)
                 | r1 <- stripPrefix "OverlayAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (TicketsAPI v1, r2)
                 | r1 <- stripPrefix "TicketsAPI " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MapAPI v1, r2)
                 | r1 <- stripPrefix "MapAPI " r,
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
            -- ++ [ (RiderRideBookingAPI v1, r2)
            --      | r1 <- stripPrefix "RIDER_RIDE_BOOKING/" r,
            --        (v1, r2) <- readsPrec (app_prec + 1) r1
            --    ]
            ++ [ (ProviderFleetAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_FLEET/" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ProviderManagementAPI v1, r2)
                 | r1 <- stripPrefix "PROVIDER_MANAGEMENT/" r,
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
castEndpoint (DMatrix.PROVIDER_MANAGEMENT uat) = ProviderManagementAPI uat
castEndpoint (DMatrix.PROVIDER_RIDE_BOOKING uat) = ProviderRideBookingAPI uat
castEndpoint (DMatrix.RIDER_MANAGEMENT uat) = RiderManagementAPI uat
-- castEndpoint (DMatrix.RIDER_RIDE_BOOKING uat) = RiderRideBookingAPI uat
castEndpoint _ = UnknownEndpoint -- should not appear
