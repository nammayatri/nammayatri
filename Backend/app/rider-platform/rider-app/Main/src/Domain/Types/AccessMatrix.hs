{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The dashboard actions this server owns. See the driver-app counterpart --
-- each server owns its own enum, so neither app depends on the other and
-- lib-dashboard names none of them.
module Domain.Types.AccessMatrix (module Domain.Types.AccessMatrix, module Reexport) where

import qualified "this" API.Types.Dashboard.AppManagement as RiderAppManagement
import qualified "this" API.Types.Dashboard.RideBooking as RiderRideBooking
import qualified "shared-services" API.Types.RiderPlatform.IssueManagement as RiderIssueManagement
import qualified "this" API.Types.RiderPlatform.Management as RiderManagement
import Data.Singletons.TH
import qualified Data.Text as T
import "lib-dashboard" Domain.Types.ServerName as Reexport (ServerName (..))
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Text.Show
import "lib-dashboard" Tools.Auth.ApiAuth as Reexport (ApiAccessLevel (..), ApiEntity (..), ApiTokenInfo (..), IsUserActionType (..), type (/))
import qualified "lib-dashboard" Tools.Auth.ApiAuth as Auth

data UserActionType
  = RIDER_MANAGEMENT RiderManagement.ManagementUserActionType
  | RIDER_APP_MANAGEMENT RiderAppManagement.AppManagementUserActionType
  | RIDER_ISSUE_MANAGEMENT RiderIssueManagement.IssueManagementUserActionType
  | RIDER_RIDE_BOOKING RiderRideBooking.RideBookingUserActionType
  | BHARAT_TAXI_FROM_LIST
  | BHARAT_TAXI_TO_LIST
  | BHARAT_TAXI_ESTIMATE
  | BHARAT_TAXI_BOOKING
  | BHARAT_TAXI_INVOICE
  | BHARAT_TAXI_BOOKING_LATEST
  | BHARAT_TAXI_BOOKING_BY_ID
  | BHARAT_TAXI_UPDATE_BOOKING
  | BHARAT_TAXI_VEHICLES_LIST
  | BHARAT_TAXI_VEHICLES_CREATE
  | BHARAT_TAXI_DRIVERS_LIST
  | BHARAT_TAXI_DRIVERS_CREATE
  deriving (Read, Generic, ToSchema, Eq, Ord)

instance Text.Show.Show UserActionType where
  show = \case
    RIDER_MANAGEMENT uat -> "RIDER_MANAGEMENT/" <> show uat
    RIDER_APP_MANAGEMENT uat -> "RIDER_APP_MANAGEMENT/" <> show uat
    RIDER_ISSUE_MANAGEMENT uat -> "RIDER_ISSUE_MANAGEMENT/" <> show uat
    RIDER_RIDE_BOOKING uat -> "RIDER_RIDE_BOOKING/" <> show uat
    BHARAT_TAXI_FROM_LIST -> "BHARAT_TAXI_FROM_LIST"
    BHARAT_TAXI_TO_LIST -> "BHARAT_TAXI_TO_LIST"
    BHARAT_TAXI_ESTIMATE -> "BHARAT_TAXI_ESTIMATE"
    BHARAT_TAXI_BOOKING -> "BHARAT_TAXI_BOOKING"
    BHARAT_TAXI_INVOICE -> "BHARAT_TAXI_INVOICE"
    BHARAT_TAXI_BOOKING_LATEST -> "BHARAT_TAXI_BOOKING_LATEST"
    BHARAT_TAXI_BOOKING_BY_ID -> "BHARAT_TAXI_BOOKING_BY_ID"
    BHARAT_TAXI_UPDATE_BOOKING -> "BHARAT_TAXI_UPDATE_BOOKING"
    BHARAT_TAXI_VEHICLES_LIST -> "BHARAT_TAXI_VEHICLES_LIST"
    BHARAT_TAXI_VEHICLES_CREATE -> "BHARAT_TAXI_VEHICLES_CREATE"
    BHARAT_TAXI_DRIVERS_LIST -> "BHARAT_TAXI_DRIVERS_LIST"
    BHARAT_TAXI_DRIVERS_CREATE -> "BHARAT_TAXI_DRIVERS_CREATE"

instance Auth.IsUserActionType UserActionType where
  showUserActionType = T.pack . show

genSingletons [''UserActionType]

type ApiAuth (sn :: DSN.ServerName) (ae :: Auth.ApiEntity) (uat :: k) = Auth.ApiAuthFor UserActionType sn ae uat
