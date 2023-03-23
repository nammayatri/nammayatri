{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Confirm
  ( buildBookingLocation,
    confirm,
    findRandomExophone,
    cancelBooking,
    buildBooking,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSRLoc
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

-- domain types

confirm :: (EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Id DQuote.Quote -> m SConfirm.DConfirmRes
confirm = SConfirm.confirm

buildBookingLocation :: MonadGuid m => UTCTime -> DSRLoc.SearchReqLocation -> m DBL.BookingLocation
buildBookingLocation = SConfirm.buildBookingLocation

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone = SConfirm.findRandomExophone

-- cancel booking when QUOTE_EXPIRED on bpp side, or other EXTERNAL_API_CALL_ERROR catched
cancelBooking :: (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r, CoreMetrics m) => DRB.Booking -> m ()
cancelBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  bookingCancellationReason <- buildBookingCancellationReason booking.id
  DB.runTransaction $ do
    QRideB.updateStatus booking.id DRB.CANCELLED
    QBCR.upsert bookingCancellationReason
  Notify.notifyOnBookingCancelled booking DBCR.ByApplication
  where
    buildBookingCancellationReason bookingId = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = bookingId,
            rideId = Nothing,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            reasonStage = Nothing,
            additionalInfo = Nothing
          }

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  DQuote.Quote ->
  DBL.BookingLocation ->
  Maybe DBL.BookingLocation ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  m DRB.Booking
buildBooking = SConfirm.buildBooking
