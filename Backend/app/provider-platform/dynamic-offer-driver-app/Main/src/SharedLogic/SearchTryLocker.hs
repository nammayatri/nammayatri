{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchTryLocker
  ( whenSearchTryCancellable,
    isSearchTryCancelled,
    isBookingCancelled,
    lockSearchTry,
    whenBookingCancellable,
    tryMarkBookingAssignmentInprogress,
    isBookingAssignmentInprogress,
    markBookingAssignmentCompleted,
    markBookingCancellationCompleted,
    markBookingReallocationStarted,
    driverScheduledHoldLockKey,
    withDriverScheduledHoldLock,
  )
where

import Domain.Types.Booking (Booking)
import Domain.Types.Person (Person)
import Domain.Types.SearchTry (SearchTry)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

isSearchTryCancelled ::
  CacheFlow m r =>
  Id SearchTry ->
  m Bool
isSearchTryCancelled searchTryId = do
  fromMaybe False <$> (Hedis.withMasterRedis $ Hedis.get (mkCancelledKey searchTryId))

lockSearchTry ::
  CacheFlow m r =>
  Id SearchTry ->
  m Bool
lockSearchTry searchTryId = do
  k <- (<= 1) <$> Hedis.incr (mkCancelledKey' searchTryId)
  when k $ Hedis.expire (mkCancelledKey' searchTryId) 5
  return k

unlockSearchTry ::
  CacheFlow m r =>
  Id SearchTry ->
  m ()
unlockSearchTry searchTryId = void $ Hedis.decr (mkCancelledKey' searchTryId)

whenSearchTryCancellable ::
  CacheFlow m r =>
  Id SearchTry ->
  m () ->
  m ()
whenSearchTryCancellable searchTryId actions = do
  gotLock <- lockSearchTry searchTryId
  if gotLock
    then do
      exep <- withTryCatch "whenSearchTryCancellable" actions
      case exep of
        Left e -> do
          unlockSearchTry searchTryId
          someExceptionToAPIErrorThrow e
        Right a -> do
          unlockSearchTry searchTryId
          pure a
    else throwError (DriverAlreadyQuoted searchTryId.getId)
  where
    someExceptionToAPIErrorThrow exc
      | Just (HTTPException err) <- fromException exc = throwError err
      | Just (BaseException err) <- fromException exc =
        throwError . InternalError . fromMaybe (show err) $ toMessage err
      | otherwise = throwError . InternalError $ show exc

mkCancelledKey :: Id SearchTry -> Text
mkCancelledKey searchTryId = "SearchTry:Cancelled:SearchTryId-" <> searchTryId.getId

mkCancelledKey' :: Id SearchTry -> Text
mkCancelledKey' searchTryId = "SearchTry:Counter:SearchTryId-" <> searchTryId.getId

isBookingCancelled ::
  CacheFlow m r =>
  Id Booking ->
  m Bool
isBookingCancelled bookingId = do
  fromMaybe False <$> (Hedis.withMasterRedis $ Hedis.get (mkBookingCancelledKey bookingId))

isBookingAssignmentInprogress ::
  CacheFlow m r =>
  Id Booking ->
  m Bool
isBookingAssignmentInprogress bookingId = do
  fromMaybe False <$> (Hedis.withMasterRedis $ Hedis.get (mkBookingAssignedKey bookingId))

-- Booking:Cancelled is tri-state: absent (no cancel), True (fresh cancel), False (cancel consumed by
-- singleBooking reallocation). A repeat of an already-consumed reassign is a duplicate; a terminate
-- cancel is let through and re-arms the marker to True so an in-flight accept is rejected.
whenBookingCancellable ::
  CacheFlow m r =>
  Id Booking ->
  Bool ->
  m a ->
  m a
whenBookingCancellable bookingId reallocateRequested actions = do
  mbCancelMarker <- Hedis.withMasterRedis $ Hedis.get @Bool (mkBookingCancelledKey bookingId)
  isBookingAssignmentInprogress' <- isBookingAssignmentInprogress bookingId
  let isDuplicateCancel = case mbCancelMarker of
        Just True -> True
        Just False -> reallocateRequested
        Nothing -> False
  if (isDuplicateCancel || isBookingAssignmentInprogress')
    then throwError (InternalError "BOOKING_CANCELLED")
    else do
      Hedis.setExp (mkBookingCancelledKey bookingId) True 120
      actions

tryMarkBookingAssignmentInprogress ::
  (CacheFlow m r, TryException m) =>
  Id Booking ->
  m Bool
tryMarkBookingAssignmentInprogress bookingId =
  Hedis.setNxExpire (mkBookingAssignedKey bookingId) 120 True

markBookingAssignmentCompleted ::
  CacheFlow m r =>
  Id Booking ->
  m ()
markBookingAssignmentCompleted bookingId = do
  Hedis.del (mkBookingAssignedKey bookingId)

-- Cleared once the reused booking is assigned (or the assignment fails), so a genuine cancel of the new
-- ride is not blocked for the rest of the TTL.
markBookingCancellationCompleted ::
  CacheFlow m r =>
  Id Booking ->
  m ()
markBookingCancellationCompleted bookingId = do
  Hedis.del (mkBookingCancelledKey bookingId)

-- Consume the triggering cancel instead of clearing it: the marker stays present so a repeat reassign is
-- still rejected, but flips to False, which the accept-side isBookingCancelled reads as "not cancelled".
markBookingReallocationStarted ::
  CacheFlow m r =>
  Id Booking ->
  m ()
markBookingReallocationStarted bookingId = do
  Hedis.setExp (mkBookingCancelledKey bookingId) False 120

mkBookingCancelledKey :: Id Booking -> Text
mkBookingCancelledKey bookingId = "Booking:Cancelled:BookingId-" <> bookingId.getId

mkBookingAssignedKey :: Id Booking -> Text
mkBookingAssignedKey bookingId = "Booking:Assigned:BookingId-" <> bookingId.getId

-- serializes a driver's concurrent scheduled accepts; the per-booking lock above cannot (two bookings = two keys)
driverScheduledHoldLockKey :: Id Person -> Text
driverScheduledHoldLockKey driverId = "Driver:ScheduledHold:DId-" <> driverId.getId

-- | Runs the action under the per-driver hold lock so a release's gate recompute cannot race an accept and lose the min.
withDriverScheduledHoldLock :: CacheFlow m r => Id Person -> m a -> m a
withDriverScheduledHoldLock driverId actions = do
  let key = driverScheduledHoldLockKey driverId
  acquireLock key
  exep <- withTryCatch "withDriverScheduledHoldLock" actions
  Hedis.unlockRedis key
  case exep of
    Left e -> someExceptionToAPIErrorThrow e
    Right a -> pure a
  where
    acquireLock key = do
      gotLock <- Hedis.tryLockRedis key 60
      unless gotLock $ do
        threadDelay 50000
        acquireLock key
    someExceptionToAPIErrorThrow exc
      | Just (HTTPException err) <- fromException exc = throwError err
      | Just (BaseException err) <- fromException exc =
        throwError . InternalError . fromMaybe (show err) $ toMessage err
      | otherwise = throwError . InternalError $ show exc
