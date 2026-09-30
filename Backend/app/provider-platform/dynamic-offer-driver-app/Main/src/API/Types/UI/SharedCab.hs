{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Types.UI.SharedCab where

-- Proxy DTOs for the driver-app `/sharedCab/*` API family.
-- Field names and JSON shapes MIRROR the driver-app ↔ app contract
-- (Plans/Shared-Cab-Plans/04-driver-side-plan.md §4, driver UI
-- sharedcab-driver-api-types.ts) — do NOT rename fields: driver-app is a
-- pure pass-through onto rider-app `/internal/sharedCab/*` (3.2).

import Data.Aeson (object, withObject, withText, (.:), (.=))
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (force, id)
import Kernel.Prelude (UTCTime)
import Kernel.Types.Common (HighPrecMoney)

-- GET /sharedCab/routes?lat&lon -------------------------------------------------

data SharedCabRoute = SharedCabRoute
  { code :: Text,
    direction :: Text,
    distanceKm :: Maybe Double,
    fromStop :: Text,
    isStandRoute :: Bool,
    name :: Text,
    toStop :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype SharedCabRoutesResp = SharedCabRoutesResp
  { routes :: [SharedCabRoute]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- POST /sharedCab/route/select ---------------------------------------------------

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

-- Either the change applied (session) or a mode is required (affectedRiders).
data SelectRouteResp = SelectRouteResp
  { affectedRiders :: Maybe [AffectedRider],
    session :: Maybe SharedCabSession
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- POST /sharedCab/seats ---------------------------------------------------------

data SeatsReq = SeatsReq
  { version :: Int,
    walkupCount :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- POST /sharedCab/booking/{bookingId}/cancel -----------------------------------

-- R54: the driver's cancel refunds the rider in full and says why.
data CancelBookingReq = CancelBookingReq
  { reason :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Session payload ---------------------------------------------------------------

data SharedCabSessionStatus = ACTIVE | ENDED | PAUSED
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabPauseReason = ABSENT | DRIVER_OFFLINE | NO_LOCATION | OFF_ROUTE
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Mirrors rider-app SessionView.RiderStatus. A status a newer rider-app adds decodes as UNKNOWN_STATUS instead of
-- failing the driver's whole session view.
data SharedCabRiderStatus = AT_STOP | MINUTES_AWAY | ARRIVING | BOARDED | UNKNOWN_STATUS
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON SharedCabRiderStatus where
  parseJSON = withText "SharedCabRiderStatus" $ \t ->
    pure $ case t of
      "AT_STOP" -> AT_STOP
      "MINUTES_AWAY" -> MINUTES_AWAY
      "ARRIVING" -> ARRIVING
      "BOARDED" -> BOARDED
      _ -> UNKNOWN_STATUS

data BoardingRider = BoardingRider
  { bookingId :: Text,
    dropStop :: Text,
    expiresAt :: Maybe UTCTime,
    fare :: HighPrecMoney,
    firstName :: Text,
    minutesAway :: Maybe Int,
    riderStatus :: SharedCabRiderStatus,
    seats :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlightingRider = AlightingRider
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RidersAtStop = RidersAtStop
  { alighting :: [AlightingRider],
    boarding :: [BoardingRider],
    stopName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DemandAtStop = DemandAtStop
  { searching :: Int,
    stopName :: Text,
    waiting :: Int,
    windowMin :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype LowDemandCard = LowDemandCard
  { alternatives :: [LowDemandAlternative]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LowDemandAlternative = LowDemandAlternative
  { distanceKm :: Double,
    name :: Text,
    riders :: Int,
    routeCode :: Text,
    windowMin :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype OffRoute = OffRoute
  { nearestRoutes :: [SharedCabRoute]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SessionRoute = SessionRoute
  { code :: Text,
    direction :: Text,
    name :: Text,
    nextStops :: [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- contract: { tag: 'AT_STOP', sinceMin, stopName } | { tag: 'MOVING' }
data SessionMovement
  = SessionMovementAtStop {sinceMin :: Int, stopName :: Text}
  | SessionMovementMoving
  deriving stock (Generic, Show, Eq)

instance ToJSON SessionMovement where
  toJSON (SessionMovementAtStop sinceMin stopName) =
    object ["tag" .= ("AT_STOP" :: Text), "sinceMin" .= sinceMin, "stopName" .= stopName]
  toJSON SessionMovementMoving = object ["tag" .= ("MOVING" :: Text)]

instance FromJSON SessionMovement where
  parseJSON = withObject "SessionMovement" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "AT_STOP" -> SessionMovementAtStop <$> o .: "sinceMin" <*> o .: "stopName"
      "MOVING" -> pure SessionMovementMoving
      other -> fail $ "Unknown session movement tag: " <> show other

instance ToSchema SessionMovement

data SharedCabSession = SharedCabSession
  { available :: Int,
    capacity :: Int,
    demandAhead :: [DemandAtStop],
    lowDemandCard :: Maybe LowDemandCard,
    movement :: SessionMovement,
    offRoute :: Maybe OffRoute,
    pauseReason :: Maybe SharedCabPauseReason,
    queuedRoute :: Maybe SessionRoute,
    ridersByStop :: [RidersAtStop],
    route :: SessionRoute,
    status :: SharedCabSessionStatus,
    version :: Int,
    walkupCount :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- POST /sharedCab/route/end -----------------------------------------------------

data EndRouteNext = CHANGE | END | RETURN
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndRouteReq = EndRouteReq
  { next :: EndRouteNext,
    atLastStop :: Maybe Bool,
    force :: Maybe Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- GET /sharedCab/trips?date ----------------------------------------------------
-- Trips-history response: {trips: [{id, routeCode, status, startedAt,
-- endedAt?, endReason?, offlineBoardings}]} — per-run summary only, no
-- riders/cash breakdown. status/endReason are BAP-owned strings (e.g.
-- COMPLETED / ENDED / ABANDONED, DRIVER_ENDED / NO_SHOWS ...).

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
