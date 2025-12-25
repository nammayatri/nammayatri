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
    markBookingAssignmentInprogress,
    isBookingAssignmentInprogress,
    markBookingAssignmentCompleted,
  )
where

import Domain.Types.Booking (Booking)
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
  Hedis.expire (mkCancelledKey' searchTryId) 5
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

whenBookingCancellable ::
  CacheFlow m r =>
  Id Booking ->
  m a ->
  m a
whenBookingCancellable bookingId actions = do
  isBookingCancelled' <- isBookingCancelled bookingId
  isBookingAssignmentInprogress' <- isBookingAssignmentInprogress bookingId
  if (isBookingCancelled' || isBookingAssignmentInprogress')
    then throwError (InternalError "BOOKING_CANCELLED")
    else do
      Hedis.setExp (mkBookingCancelledKey bookingId) True 120
      actions

markBookingAssignmentInprogress ::
  CacheFlow m r =>
  Id Booking ->
  m ()
markBookingAssignmentInprogress bookingId = do
  Hedis.setExp (mkBookingAssignedKey bookingId) True 120

markBookingAssignmentCompleted ::
  CacheFlow m r =>
  Id Booking ->
  m ()
markBookingAssignmentCompleted bookingId = do
  Hedis.del (mkBookingAssignedKey bookingId)

mkBookingCancelledKey :: Id Booking -> Text
mkBookingCancelledKey bookingId = "Booking:Cancelled:BookingId-" <> bookingId.getId

mkBookingAssignedKey :: Id Booking -> Text
mkBookingAssignedKey bookingId = "Booking:Assigned:BookingId-" <> bookingId.getId
