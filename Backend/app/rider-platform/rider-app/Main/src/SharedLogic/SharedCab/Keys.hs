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
    declineCountKey,
    riderFixKey,
    cancelReasonKey,
    degradedKey,
    dropClockKey,
    refundKey,
    refundRetryKey,
    boardAttemptsKey,
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

noLocationKey :: Text -> Text -> Text
noLocationKey plate istDay = "sharedcab:noloc:" <> plate <> ":" <> istDay

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

-- | How many offers a driver declined or let lapse on a day (YYYYMMDD); counted for later, nothing reads it yet.
declineCountKey :: Text -> Text -> Text
declineCountKey driverId day = "sharedcab:declines:" <> driverId <> ":" <> day

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
