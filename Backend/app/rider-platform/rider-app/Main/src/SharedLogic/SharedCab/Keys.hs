-- | Every rider-app shared-cab Redis key and lock name (the driver app's one key, the reconciler seed marker, lives in its ReconcilerSeed). They are unprefixed and live in the cross-app master cell
-- (`Booking.shared`), because the API and the scheduler use different key prefixes and must see the same keys.
module SharedLogic.SharedCab.Keys
  ( -- * Session (per plate / route)
    sessionKey,
    routeKey,
    driverInfoKey,
    plateLockKey,
    offRouteKey,
    noLocationKey,
    bookingLockKey,
    bookingStateKey,
    fallbackPushedKey,
    pickedCabKey,
    interestKey,
    riderFixKey,
    cancelReasonKey,
    degradedKey,
    dropClockKey,
    refundKey,
    refundRetryKey,
    boardAttemptsKey,
    chatAttemptsKey,
    allocLeaseKey,
    refundRetryCityKey,
    demandKey,
    allocJobGuardKey,
    allocRunKey,
    expiryJobGuardKey,
    expiryTickKey,
    sweepJobGuardKey,
    sweepRunKey,
    sweepLeaseKey,
    refundRetryRunKey,
    seatHoldKey,
    walkupSeatsKey,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id

type BookingId = Id DFTB.FRFSTicketBooking

type CityId = Id DMOC.MerchantOperatingCity

sessionKey :: Text -> Text
sessionKey plate = "sharedcab:session:" <> plate

routeKey :: Text -> Text
routeKey routeCode = "sharedcab:route:" <> routeCode

driverInfoKey :: Text -> Text
driverInfoKey plate = "sharedcab:driverinfo:" <> plate

plateLockKey :: Text -> Text
plateLockKey plate = "sharedcab:lock:" <> plate

offRouteKey :: Text -> Text
offRouteKey plate = "sharedcab:offroute:" <> plate

noLocationKey :: Text -> Text
noLocationKey plate = "sharedcab:noloc:" <> plate

bookingLockKey :: BookingId -> Text
bookingLockKey bookingId = "sharedcab:lock:booking:" <> bookingId.getId

-- | One hash per booking (fields in BookingState): the alloc record, lifetime attempts, finding-since and skipped plates.
bookingStateKey :: BookingId -> Text
bookingStateKey bookingId = "sharedcab:booking:" <> bookingId.getId

-- | A once-flag, so it needs SETNX and stays outside the hash.
fallbackPushedKey :: BookingId -> Text
fallbackPushedKey bookingId = "sharedcab:fallbackpushed:" <> bookingId.getId

-- | The cab the rider picked, held from the confirm request until the booking exists (keyed by the search the booking is made from).
pickedCabKey :: Text -> Text
pickedCabKey searchId = "sharedcab:pickedcab:" <> searchId

-- | Riders who opened a stand's cab view and have not booked: person id scored by when, per route.
interestKey :: Text -> Text
interestKey routeCode = "sharedcab:interest:" <> routeCode

-- | The rider's latest position, for the tick's passed-stop judgement.
riderFixKey :: BookingId -> Text
riderFixKey bookingId = "sharedcab:riderfix:" <> bookingId.getId

cancelReasonKey :: BookingId -> Text
cancelReasonKey bookingId = "sharedcab:cancelreason:" <> bookingId.getId

degradedKey :: BookingId -> Text
degradedKey bookingId = "sharedcab:degraded:" <> bookingId.getId

dropClockKey :: BookingId -> Text
dropClockKey bookingId = "sharedcab:dropclock:" <> bookingId.getId

refundKey :: BookingId -> Text
refundKey bookingId = "sharedcab:refund:" <> bookingId.getId

refundRetryKey :: BookingId -> Text
refundRetryKey bookingId = "sharedcab:refundretry:" <> bookingId.getId

-- | Per booking: a booking belongs to exactly one rider, so this bounds the rider's probe rate.
boardAttemptsKey :: BookingId -> Text
boardAttemptsKey bookingId = "sharedcab:rate:board:" <> bookingId.getId

-- | Per booking and driver: bounds how many chat pushes one driver can send one rider.
chatAttemptsKey :: BookingId -> Text -> Text
chatAttemptsKey bookingId driverId = "sharedcab:rate:chat:" <> bookingId.getId <> ":" <> driverId

allocLeaseKey :: CityId -> Text
allocLeaseKey cityId = "sharedcab:alloc:lease:" <> cityId.getId

refundRetryCityKey :: CityId -> Text
refundRetryCityKey cityId = "sharedcab:refundretry:city:" <> cityId.getId

demandKey :: Text -> Text -> Int -> Text
demandKey cityId stopCode bucket = "sharedcab:demand:" <> cityId <> ":" <> stopCode <> ":" <> show bucket

allocJobGuardKey :: CityId -> Text
allocJobGuardKey cityId = "sharedcab:allocJob:" <> cityId.getId

allocRunKey :: CityId -> Text
allocRunKey cityId = "sharedcab:allocRun:" <> cityId.getId

expiryJobGuardKey :: CityId -> Text
expiryJobGuardKey cityId = "sharedcab:expiryJob:" <> cityId.getId

expiryTickKey :: CityId -> Text
expiryTickKey cityId = "sharedcab:expiryTick:" <> cityId.getId

sweepJobGuardKey :: CityId -> Text
sweepJobGuardKey cityId = "sharedcab:degradedSweepJob:" <> cityId.getId

sweepRunKey :: CityId -> Text
sweepRunKey cityId = "sharedcab:degradedSweepRun:" <> cityId.getId

sweepLeaseKey :: CityId -> Text
sweepLeaseKey cityId = "sharedcab:degradedSweepLease:" <> cityId.getId

refundRetryRunKey :: CityId -> Text
refundRetryRunKey cityId = "sharedcab:refundretryRun:" <> cityId.getId

-- | The seats a booking's party holds in the seat engine (a SeatHoldMeta), so a release gives back exactly those.
seatHoldKey :: BookingId -> Text
seatHoldKey bookingId = "sharedcab:seats:" <> bookingId.getId

-- | A trip's walk-up seats, one hold per walk-up.
walkupSeatsKey :: Text -> Text
walkupSeatsKey tripId = "sharedcab:walkups:" <> tripId
