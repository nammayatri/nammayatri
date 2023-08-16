{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.CustomerStats.Processor
  ( updateCustomerStats,
  )
where

import "rider-app" Domain.Types.BookingCancellationReason as SBCR
import "dynamic-offer-driver-app" Domain.Types.Ride as DDR
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import "sessionizer-metrics" Lib.SessionizerMetrics.Types.Event as E
import "rider-app" SharedLogic.Person as SP
import "rider-app" Storage.CachedQueries.Merchant as CQMerchant
import "rider-app" Storage.Queries.BookingCancellationReason as QBCR
import "rider-app" Storage.Queries.Person.PersonStats as QP
import "rider-app" Tools.Error
import "dynamic-offer-driver-app" Tools.Event as TE

updateCustomerStats :: E.Event TE.Payload -> Text -> Flow ()
updateCustomerStats event _ = do
  whenJust event.personId $ \pId -> do
    when (event.service == RIDER_APP) do
      let personId = Id pId
          merchantId = Id event.merchantId
      -- personStats <- Esq.runInReplica $ QP.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
      personStats <- runInReplica $ QP.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
      when (isNotBackfilled personStats) do
        personStatsValues <- SP.backfillPersonStats personId merchantId
        -- Esq.runNoTransaction $ QP.incrementOrSetPersonStats personStatsValues
        QP.incrementOrSetPersonStats personStatsValues
      whenJust (event.payload) $ \payload -> do
        unless (isNotBackfilled personStats) do
          case event.eventType of
            RideEnded -> do
              case payload.rs of
                DDR.COMPLETED -> do
                  let createdAt = payload.cAt
                  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
                  let minuteDiffFromUTC = (merchant.timeDiffFromUtc.getSeconds) `div` 60
                  -- Esq.runNoTransaction $ do
                  when (SP.isWeekend createdAt minuteDiffFromUTC) do QP.incrementWeekendRidesCount personId
                  unless (SP.isWeekend createdAt minuteDiffFromUTC) do QP.incrementWeekdayRidesCount personId
                  when (SP.checkMorningPeakInWeekday createdAt minuteDiffFromUTC) do QP.incrementMorningPeakRidesCount personId
                  when (SP.checkEveningPeakInWeekday createdAt minuteDiffFromUTC) do QP.incrementEveningPeakRidesCount personId
                  when (SP.checkWeekendPeak createdAt minuteDiffFromUTC) do QP.incrementWeekendPeakRidesCount personId
                  when (not (SP.checkEveningPeakInWeekday createdAt minuteDiffFromUTC) && not (SP.checkMorningPeakInWeekday createdAt minuteDiffFromUTC) && not (SP.checkWeekendPeak createdAt minuteDiffFromUTC)) do QP.incrementOffpeakRidesCount personId
                  QP.incrementCompletedRidesCount personId
                _ -> pure ()
            BookingCancelled -> do
              let bookingId = cast payload.bId
              -- bookingCancellationReason <- Esq.runInReplica $ QBCR.findByRideBookingId bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
              bookingCancellationReason <- runInReplica $ QBCR.findByRideBookingId bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
              -- Esq.runNoTransaction $ do
              when (bookingCancellationReason.source == SBCR.ByUser) do QP.incrementUserCancelledRidesCount personId
              when (bookingCancellationReason.source == SBCR.ByDriver) do QP.incrementDriverCancelledRidesCount personId
            _ -> pure ()
  where
    isNotBackfilled personStats_ = all (== 0) [personStats_.userCancelledRides, personStats_.completedRides, personStats_.weekendRides, personStats_.weekdayRides, personStats_.offPeakRides, personStats_.eveningPeakRides, personStats_.morningPeakRides, personStats_.weekendPeakRides]
