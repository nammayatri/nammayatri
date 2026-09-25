module SharedLogic.SharedCab.Booking
  ( isSharedCabBooking,
    withBookingLock,
    ensureCancellable,
    markDropped,
    ridersOnBoard,
    liveSeatsOnVehicle,
    findingOnRoute,
    shared,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils (getServiceTierTypeFromRouteStationsJson)
import SharedLogic.SharedCab.LegState (isDroppable, seatsHeld)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

isSharedCabBooking :: DFRFSTicketBooking.FRFSTicketBooking -> Bool
isSharedCabBooking booking = getServiceTierTypeFromRouteStationsJson booking.routeStationsJson == Just Spec.SHARED_CAB

-- | The cross-app master cell, unprefixed: allocation (`sharedcab:alloc:`) and degraded-boarding (`sharedcab:degraded:`)
-- keys live here so every app and the scheduler see them. Session keys and the plate lock stay app-prefixed.
shared :: (Redis.HedisFlow m r, MonadFlow m) => m a -> m a
shared = Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis

-- | `05` §2: every write that moves a shared-cab booking between states runs under this lock.
withBookingLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m a
withBookingLock bookingId =
  -- cross-app: the allocation tick (scheduler) and the API take this lock on the same booking
  Redis.withWaitAndLockMasterCloudCrossAppRedis "sharedCab" "waitForBookingLock" ("sharedcab:lock:booking:" <> bookingId.getId) 10 10000

-- | R7: no cancel once a seat has boarded. Call inside `withBookingLock` so boarding can't slip in before the cancel.
ensureCancellable :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DFRFSTicketBooking.FRFSTicketBooking -> m ()
ensureCancellable booking = do
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  when (any ((== DFRFSTicket.INPROGRESS) . (.status)) tickets) $
    throwError $ InvalidRequest "This shared cab ride has started and can't be cancelled"

-- | "I got down" (R8): tickets still held go USED, which ends the leg and takes the seat out of the cab's live set.
-- TODO(7.4): clear sharedcab:alloc:{bookingId} and emit the drop event once allocation keys exist.
markDropped :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => DFRFSTicketBooking.FRFSTicketBooking -> m ()
markDropped booking = withBookingLock booking.id $ do
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  forM_ (filter (isDroppable . (.status)) tickets) $ \ticket ->
    QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.USED ticket.scannedByVehicleNumber booking.id ticket.ticketNumber

-- | `04` §4: the cab's bookings with a seat on board (a ticket INPROGRESS). `plate` is canonical.
ridersOnBoard :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
ridersOnBoard plate = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFRFSTicketBookingStatus.CONFIRMED]
  if null bookings
    then pure []
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
      let boarded = [ticket.frfsTicketBookingId | ticket <- tickets, ticket.status == DFRFSTicket.INPROGRESS]
      pure $ filter ((`elem` boarded) . (.id)) bookings

-- | `04` §3: seats app bookings hold on the cab, one per ticket still held (ALLOCATED or BOARDED). A degraded
-- boarding holds none (`05` §5); its marker is cross-app, like the allocation engine's keys. `plate` is canonical.
liveSeatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Int
liveSeatsOnVehicle plate = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFRFSTicketBookingStatus.CONFIRMED]
  counted <- filterM (fmap isNothing . shared . Redis.get @A.Value . ("sharedcab:degraded:" <>) . getId . (.id)) bookings
  if null counted
    then pure 0
    else seatsHeld . map (.status) <$> QFRFSTicket.findAllByTicketBookingIds (map (.id) counted)

-- | `05` §3: FINDING = CONFIRMED with no cab yet.
findingOnRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
findingOnRoute routeCode =
  filter (isNothing . (.vehicleNumber))
    <$> QFRFSTicketBooking.findAllByRouteCodeAndServiceTierTypeAndStatus (Just routeCode) (Just Spec.SHARED_CAB) DFRFSTicketBookingStatus.CONFIRMED
