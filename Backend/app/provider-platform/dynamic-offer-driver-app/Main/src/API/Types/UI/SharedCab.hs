{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Types.UI.SharedCab (module API.Types.UI.SharedCab, module Domain.Types.SharedCabSession) where

-- Proxy DTOs for the driver-app `/sharedCab/*` API: a pure pass-through onto rider-app `/internal/sharedCab/*`, so do not
-- rename fields.

import Data.Aeson (withText)
import Data.OpenApi (ToSchema)
import Domain.Types.SharedCabSession
import EulerHS.Prelude hiding (force, id)
import Kernel.Prelude (UTCTime)

newtype SharedCabRoutesResp = SharedCabRoutesResp
  { routes :: [SharedCabRoute]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectRouteMode
  = AfterLastDrop
  | Force
  deriving stock (Generic, Show, Eq)

-- contract: mode?: 'afterLastDrop' | 'force'
instance ToJSON SelectRouteMode where
  toJSON AfterLastDrop = "afterLastDrop"
  toJSON Force = "force"

instance FromJSON SelectRouteMode where
  parseJSON = withText "SelectRouteMode" $ \case
    "afterLastDrop" -> pure AfterLastDrop
    "force" -> pure Force
    tag -> fail $ "Invalid select-route mode: " <> show tag

instance ToSchema SelectRouteMode

data SelectRouteReq = SelectRouteReq
  { mode :: Maybe SelectRouteMode,
    routeCode :: Text,
    walkupCount :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AffectedRider = AffectedRider
  { bookingId :: Text,
    dropStop :: Text,
    firstName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectRouteResp = SelectRouteResp
  { affectedRiders :: Maybe [AffectedRider],
    session :: Maybe SharedCabSession
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatsReq = SeatsReq
  { version :: Int,
    walkupCount :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancelBookingReq = CancelBookingReq
  { reason :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndRouteReq = EndRouteReq
  { next :: EndRouteNext,
    atLastStop :: Maybe Bool,
    force :: Maybe Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Trips-history response: per-run summary only. status/endReason are BAP-owned strings.

data SharedCabTrip = SharedCabTrip
  { id :: Text,
    routeCode :: Text,
    status :: Text,
    startedAt :: UTCTime,
    endedAt :: Maybe UTCTime,
    endReason :: Maybe Text,
    offlineBoardings :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype SharedCabTripsResp = SharedCabTripsResp
  { trips :: [SharedCabTrip]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
