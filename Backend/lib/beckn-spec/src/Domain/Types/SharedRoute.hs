{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The knobs that make one shared-route service out of the common run-and-ticket spine: a bus line, a
-- corporate shuttle, a shared cab, a works bus are all the same code under different knob values.
--
-- Two records, one per side. The demand knobs live on the rider platform, as JSON inside the feed's
-- @integrated_bpp_config.provider_config@ (@DIRECTConfig.sharedKnobs@): they say how a passenger books,
-- holds a seat, boards and cancels. The supply knobs live on the driver platform, as JSON on the city's
-- @transporter_config.shared_run_config@: they say how a run is staffed and paid.
--
-- Every value exists from day one so a new product is a config change. Code dispatching on a knob may
-- still refuse a value nobody has needed yet (see @notImplemented@ in each platform's Knobs module), and
-- 'validateDemandKnobs' / 'validateSupplyKnobs' reject pairs that make no sense together.
module Domain.Types.SharedRoute where

import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude

-- | How much of the path is fixed. FIXED follows the GTFS shape stop by stop; CORRIDOR keeps the stops but
-- lets the driver pick the way between them; FREE is only a start and an end (not used by any product yet).
data RouteShape = FIXED | CORRIDOR | FREE
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Where the departure times come from. TIMETABLED runs exist in the GTFS calendar before anyone books;
-- HEADWAY runs leave every N minutes while there is a vehicle; ON_DEMAND runs start when a driver opens one.
data ScheduleMode = TIMETABLED | HEADWAY | ON_DEMAND
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | UNLIMITED never refuses a ticket (city bus, metro). CAPACITY counts riders against the vehicle's seats
-- per stop range (shared cab). RESERVED hands out numbered seats (shuttle, intercity bus). MIXED reserves
-- some and keeps the rest as capacity.
data SeatMode = UNLIMITED | CAPACITY | RESERVED | MIXED
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | PRE_BOOK: a ticket before the run leaves. WALK_ON: no ticket in the app, the rider just boards and the
-- driver counts them. BOTH: either. INSTANT: book and board within minutes, the cab is already moving.
data BookingMode = PRE_BOOK | WALK_ON | BOTH | INSTANT
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Who binds a passenger to a vehicle. NO_ALLOCATION: the rider boards whatever comes. PRE_ASSIGNED: the
-- ticket is on one scheduled run and the driver platform staffs that run. LIVE: the rider platform picks a
-- moving cab for each booking and may change its mind (shared cab).
data AllocationMode = NO_ALLOCATION | PRE_ASSIGNED | LIVE
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | What proves the rider got on. GATE: the operator's own gate, nothing in our apps. CONDUCTOR_SCAN: a
-- conductor scans the ticket QR. PLATE_CHECKIN: the rider enters the vehicle number. STICKER_CODE: the rider
-- types the code printed in the cab. OTP: the driver enters the rider's per-booking code. NO_CHECK: nobody checks.
data BoardingMode = GATE | CONDUCTOR_SCAN | PLATE_CHECKIN | STICKER_CODE | OTP | NO_CHECK
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | What the platform takes from the driver for a run. PER_TICKET: the fare policy's commission on the sum of
-- ticket fares. PER_RUN: a flat amount per run. PLAN_ONLY: nothing per run, the driver pays a subscription
-- plan. NO_FEE: nothing at all (an owned fleet on salary).
data DriverFeeBasis = PER_TICKET | PER_RUN | PLAN_ONLY | NO_FEE
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | What a missed boarding costs the rider. FORFEIT_TRIP: one trip comes off the subscription counter.
-- FORFEIT_FARE: the ticket is consumed without refund. STRIKE: counted towards a block, no money moves.
data NoShowConsequence = NO_CONSEQUENCE | FORFEIT_TRIP | FORFEIT_FARE | STRIKE
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Where a passenger of one run stands, as the driver's manifest shows it. Encoded on the rider platform's
-- @frfs_ticket_booking@ and its tickets, no new column:
--
-- * WAITING: @ride_id@ set, no ticket INPROGRESS or USED, booking CONFIRMED.
-- * BOARDED: a ticket INPROGRESS (the driver boarded them, by OTP, code or plain tap).
-- * NO_SHOW: @shared_cab_no_shows@ above zero on this run and nothing boarded.
-- * DROPPED: a ticket USED (the driver dropped them, or the run ended with them aboard).
-- * CANCELLED: booking status CANCELLED; the manifest leaves these out.
data SharedPassengerState = WAITING | BOARDED | NO_SHOW | DROPPED | CANCELLED
  deriving stock (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Demand-side knobs, on the rider platform per feed (integrated bpp config).
data SharedDemandKnobs = SharedDemandKnobs
  { route :: RouteShape,
    schedule :: ScheduleMode,
    seats :: SeatMode,
    booking :: BookingMode,
    allocation :: AllocationMode,
    boarding :: BoardingMode,
    -- | Minutes before departure until which a rider may cancel without the no-show consequence.
    -- Nothing: cancel is free until departure.
    cancelCutoffMinutes :: Maybe Int,
    noShowConsequence :: NoShowConsequence
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Supply-side knobs, on the driver platform per city (transporter config).
data SharedSupplyKnobs = SharedSupplyKnobs
  { schedule :: ScheduleMode,
    -- | GTFS feed keys (integrated bpp config @feed_key@, same on both platforms) whose timetables this city
    -- turns into runs. The materialiser reads every route of every feed listed here; a feed not listed is
    -- somebody else's (a government bus feed GIMS also serves, say).
    feeds :: [Text],
    driverFee :: DriverFeeBasis,
    -- | Minutes two runs of one driver must keep apart, over and above travel time between them.
    clashWindowMinutes :: Int,
    -- | Minutes before departure at which an unacknowledged run is handed to the fallback job.
    fallbackWindowMinutes :: Int,
    -- | Minutes before departure at which the assigned driver is asked to acknowledge the run. 0: never asked.
    checkInOffsetMinutes :: Int,
    -- | Seconds the driver waits at a stop for a booked rider who has not boarded before marking a no-show.
    noShowWaitSeconds :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Pairs of demand knobs that cannot work together. Checked when the knobs are read, so a bad config fails
-- loudly at the first booking rather than quietly at boarding.
validateDemandKnobs :: SharedDemandKnobs -> Either Text ()
validateDemandKnobs k =
  firstFailure
    [ (k.seats `elem` [RESERVED, MIXED] && k.booking == WALK_ON, "RESERVED or MIXED seats need a booking before boarding; WALK_ON has none"),
      (k.allocation == LIVE && k.schedule == TIMETABLED, "LIVE allocation picks a moving vehicle; a TIMETABLED run is fixed before anyone books"),
      (k.allocation == PRE_ASSIGNED && k.schedule == ON_DEMAND, "PRE_ASSIGNED needs a run that exists before the booking; ON_DEMAND runs do not"),
      (k.boarding == OTP && k.booking == WALK_ON, "OTP boarding needs a booking to carry the OTP; WALK_ON has none"),
      (k.route == FREE && k.schedule == TIMETABLED, "a FREE route has no stops to timetable"),
      (maybe False (< 0) k.cancelCutoffMinutes, "cancelCutoffMinutes must not be negative"),
      (k.noShowConsequence /= NO_CONSEQUENCE && k.booking == WALK_ON, "a no-show consequence needs a booking to apply it to; WALK_ON has none")
    ]

-- | Supply knobs that cannot work together.
validateSupplyKnobs :: SharedSupplyKnobs -> Either Text ()
validateSupplyKnobs k =
  firstFailure
    [ (k.schedule == TIMETABLED && null k.feeds, "a TIMETABLED city needs at least one GTFS feed in feeds to read its timetable from"),
      (any T.null k.feeds, "feeds must not contain an empty feed key"),
      (k.clashWindowMinutes < 0, "clashWindowMinutes must not be negative"),
      (k.fallbackWindowMinutes < 0, "fallbackWindowMinutes must not be negative"),
      (k.checkInOffsetMinutes < 0, "checkInOffsetMinutes must not be negative"),
      (k.noShowWaitSeconds < 0, "noShowWaitSeconds must not be negative"),
      (k.checkInOffsetMinutes > 0 && k.fallbackWindowMinutes > k.checkInOffsetMinutes, "the fallback cannot fire before the driver was even asked to check in: fallbackWindowMinutes must not exceed checkInOffsetMinutes"),
      (k.schedule == ON_DEMAND && k.checkInOffsetMinutes > 0, "an ON_DEMAND run has no departure to check in for; set checkInOffsetMinutes to 0")
    ]

firstFailure :: [(Bool, Text)] -> Either Text ()
firstFailure checks = case [msg | (failed, msg) <- checks, failed] of
  [] -> Right ()
  msgs -> Left (T.intercalate "; " msgs)
