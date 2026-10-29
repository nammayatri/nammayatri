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
    notifyBookingCancelled,
    notifyFindingTimeout,
    notifyRouteChange,
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

data SharedCabNotificationType
  = SHARED_CAB_ASSIGNED
  | SHARED_CAB_ARRIVING
  | SHARED_CAB_REASSIGNED
  | SHARED_CAB_BOARD_ANY
  | SHARED_CAB_ROUTE_CHANGE
  | SHARED_CAB_BOOKING_CANCELLED
  | SHARED_CAB_FINDING_TIMEOUT
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

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

templateParams :: (Maybe Text, Text) -> (Maybe Text, Text) -> Maybe Text -> [(Text, Text)]
templateParams (boardName, boardCode) (dropName, dropCode) mbVehicleNumber =
  [("boardStop", fromMaybe boardCode boardName), ("dropStop", fromMaybe dropCode dropName)]
    <> maybe [] (\v -> [("vehicleNumber", v)]) mbVehicleNumber

-- | Forked, and every failure only logged: a push never fails the rider's or driver's request. `extraParams` are appended
-- to `templateParams` for copy that needs more (the arrival countdown).
send ::
  (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) =>
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

notifyAssigned :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => Text -> DFTB.FRFSTicketBooking -> m ()
notifyAssigned plate = send SHARED_CAB_ASSIGNED Nothing Nothing (Just plate) []

-- | The allocated cab is within `atStopRadiusM` of the board stop. `deadlineSec` is the timer just armed: the countdown the app shows.
notifyArriving :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => Text -> Int -> DFTB.FRFSTicketBooking -> m ()
notifyArriving plate deadlineSec = send SHARED_CAB_ARRIVING Nothing Nothing (Just plate) [("boardDeadlineSec", show deadlineSec), ("vehicleLast4", T.takeEnd 4 plate)]

notifyReassigned :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => ReassignReason -> DFTB.FRFSTicketBooking -> m ()
notifyReassigned reason = send SHARED_CAB_REASSIGNED Nothing (Just reason) Nothing []

reassignReasonFor :: AllocationOutcome -> Maybe ReassignReason
reassignReasonFor = \case
  StandTimeout -> Just TIMEOUT
  MovingTimeout -> Just TIMEOUT
  AwayTimeout -> Just CAB_PULLED
  SeatLost -> Just SEAT_LOST
  DriverCancelled -> Just CAB_PULLED
  PassedStop _ -> Just CAB_PULLED
  RouteChanged -> Just CAB_PULLED
  SessionClosed -> Just CAB_PULLED
  TimerLost -> Just CAB_PULLED
  CabSilent -> Just CAB_PULLED
  DriverDeclined -> Nothing
  OfferTimeout -> Nothing
  RiderSkipped _ -> Nothing

notifyBoardAny :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => DFTB.FRFSTicketBooking -> m ()
notifyBoardAny = send SHARED_CAB_BOARD_ANY Nothing Nothing Nothing []

notifyBookingCancelled :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => Int -> DFTB.FRFSTicketBooking -> m ()
notifyBookingCancelled missedCabs = send SHARED_CAB_BOOKING_CANCELLED Nothing Nothing Nothing [("missedCabs", show missedCabs)]

notifyFindingTimeout :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => DFTB.FRFSTicketBooking -> m ()
notifyFindingTimeout = send SHARED_CAB_FINDING_TIMEOUT Nothing Nothing Nothing []

notifyRouteChange :: (ServiceFlow m r, MonadFlow m, HasFlowEnv m r '["maxNotificationShards" ::: Int]) => Text -> DFTB.FRFSTicketBooking -> m ()
notifyRouteChange routeCode = send SHARED_CAB_ROUTE_CHANGE (Just routeCode) Nothing Nothing []
