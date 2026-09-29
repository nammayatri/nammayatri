module SharedLogic.SharedCab.Notify
  ( SharedCabNotificationType (..),
    ReassignReason (..),
    SharedCabNotificationEntityData (..),
    notificationKey,
    templateParams,
    notifyAssigned,
    notifyArriving,
    notifyReassigned,
    reassignReasonFor,
    notifyBoardAny,
    notifyRouteChange,
    notifyDropConfirm,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Notifications (createNotificationReq, dynamicNotifyPerson)

-- | `07` B10. The constructor name is the merchant_push_notification key and the entity's notificationType.
data SharedCabNotificationType
  = SHARED_CAB_ASSIGNED
  | SHARED_CAB_ARRIVING
  | SHARED_CAB_REASSIGNED
  | SHARED_CAB_BOARD_ANY
  | SHARED_CAB_ROUTE_CHANGE
  | SHARED_CAB_DROP_CONFIRM
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | TIMEOUT: the rider's own timer ran out. SEAT_LOST: a walk-up took the seat. CAB_PULLED: the cab went away (driver cancel,
-- route change, session paused/ended, cab passed the stop).
data ReassignReason = SEAT_LOST | TIMEOUT | CAB_PULLED
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SharedCabNotificationEntityData = SharedCabNotificationEntityData
  { notificationType :: SharedCabNotificationType,
    bookingId :: Text,
    searchId :: Text,
    vehicleNumber :: Maybe Text,
    routeCode :: Maybe Text,
    reassignReason :: Maybe ReassignReason
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notificationKey :: SharedCabNotificationType -> Text
notificationKey = show

-- | Every type gets the same params, so the copy in merchant_push_notification can use any of them.
-- Stops are `(name, code)`; the code stands in when the name is missing.
templateParams :: (Maybe Text, Text) -> (Maybe Text, Text) -> Maybe Text -> [(Text, Text)]
templateParams (boardName, boardCode) (dropName, dropCode) mbVehicleNumber =
  [("boardStop", fromMaybe boardCode boardName), ("dropStop", fromMaybe dropCode dropName)]
    <> maybe [] (\v -> [("vehicleNumber", v)]) mbVehicleNumber

-- | Forked, and every failure only logged: a push never fails the rider's or driver's request.
-- `extraParams` are appended to `templateParams` (boardStop/dropStop/vehicleNumber), for copy that
-- needs more than those three (R17's countdown).
send ::
  (ServiceFlow m r, MonadFlow m) =>
  SharedCabNotificationType ->
  Maybe Text ->
  Maybe ReassignReason ->
  Maybe Text ->
  [(Text, Text)] ->
  DFTB.FRFSTicketBooking ->
  m ()
send notificationType mbRouteCode reassignReason mbVehicleNumber extraParams booking =
  fork tag $
    withTryCatch tag push >>= either (\err -> logError $ tag <> " failed: " <> show err) pure
  where
    tag = "sharedCab:notify:" <> notificationKey notificationType <> ":" <> booking.id.getId
    vehicleNumber = mbVehicleNumber <|> booking.vehicleNumber
    entityData =
      SharedCabNotificationEntityData
        { notificationType,
          bookingId = booking.id.getId,
          searchId = booking.searchId.getId,
          vehicleNumber,
          routeCode = mbRouteCode,
          reassignReason
        }
    push =
      QPerson.findById booking.riderId >>= \case
        Nothing -> logError $ tag <> ": rider " <> booking.riderId.getId <> " not found"
        Just person ->
          dynamicNotifyPerson
            person
            (createNotificationReq (notificationKey notificationType) identity)
            EmptyDynamicParam
            (Notification.Entity Notification.Product person.id.getId entityData)
            Nothing
            (templateParams (booking.fromStationName, booking.fromStationCode) (booking.toStationName, booking.toStationCode) vehicleNumber <> extraParams)
            Nothing
            Nothing

-- | Allocation (`05` §8): a cab took the booking. `plate` is the allocated cab's.
notifyAssigned :: (ServiceFlow m r, MonadFlow m) => Text -> DFTB.FRFSTicketBooking -> m ()
notifyAssigned plate = send SHARED_CAB_ASSIGNED Nothing Nothing (Just plate) []

-- | R17: the allocated cab is within `atStopRadiusM` of the board stop (stand timer armed at a
-- stationary claim, or the moving timer armed by stop-progress). `deadlineSec` is the timer just
-- armed (standTimerSec / movingTimerSec) -- the countdown the app shows, "board within Xs".
notifyArriving :: (ServiceFlow m r, MonadFlow m) => Text -> Int -> DFTB.FRFSTicketBooking -> m ()
notifyArriving plate deadlineSec = send SHARED_CAB_ARRIVING Nothing Nothing (Just plate) [("boardDeadlineSec", show deadlineSec), ("vehicleLast4", T.takeEnd 4 plate)]

-- | The allocation was released (seat lost to a walk-up, or its timer ran out); the booking is FINDING again.
notifyReassigned :: (ServiceFlow m r, MonadFlow m) => ReassignReason -> DFTB.FRFSTicketBooking -> m ()
notifyReassigned reason = send SHARED_CAB_REASSIGNED Nothing (Just reason) Nothing []

-- | F7: the push a released allocation owes the rider. The rider's own skip owes none.
reassignReasonFor :: AllocationOutcome -> Maybe ReassignReason
reassignReasonFor = \case
  StandTimeout -> Just TIMEOUT
  MovingTimeout -> Just TIMEOUT
  SeatLost -> Just SEAT_LOST
  DriverCancelled -> Just CAB_PULLED
  PassedStop _ -> Just CAB_PULLED
  RouteChanged -> Just CAB_PULLED
  SessionClosed -> Just CAB_PULLED
  TimerLost -> Just CAB_PULLED
  CabSilent -> Just CAB_PULLED
  RiderSkipped _ -> Nothing

-- | R10: allocation gave up; any cab on the route will do.
notifyBoardAny :: (ServiceFlow m r, MonadFlow m) => DFTB.FRFSTicketBooking -> m ()
notifyBoardAny = send SHARED_CAB_BOARD_ANY Nothing Nothing Nothing []

-- | R13: the rider's cab is switching to `routeCode` before reaching their drop stop.
notifyRouteChange :: (ServiceFlow m r, MonadFlow m) => Text -> DFTB.FRFSTicketBooking -> m ()
notifyRouteChange routeCode = send SHARED_CAB_ROUTE_CHANGE (Just routeCode) Nothing Nothing []

-- | R8: "Did you get down?", answered by the "I got down" call.
notifyDropConfirm :: (ServiceFlow m r, MonadFlow m) => DFTB.FRFSTicketBooking -> m ()
notifyDropConfirm = send SHARED_CAB_DROP_CONFIRM Nothing Nothing Nothing []
