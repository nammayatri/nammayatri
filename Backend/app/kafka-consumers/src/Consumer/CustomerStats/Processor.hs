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
import "rider-app" Storage.CachedQueries.Merchant.MerchantConfigNew as CQMC
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
        SP.backfillPersonStats personId merchantId
      whenJust (event.payload) $ \payload -> do
        unless (isNotBackfilled personStats) do
          case event.eventType of
            RideEnded -> do
              case payload.rs of
                DDR.COMPLETED -> do
                  let createdAt = payload.cAt
                  merchantConfig <- CQMC.findByMerchantId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
                  let minuteDiffFromUTC = (merchantConfig.timeDiffFromUtc.getSeconds) `div` 60
                  -- Esq.runNoTransaction $ do
                  if SP.isWeekend createdAt minuteDiffFromUTC
                    then do QP.incrementWeekendRidesCount personId
                    else do QP.incrementWeekdayRidesCount personId
                  let isMorningPeak = SP.checkMorningPeakInWeekday createdAt minuteDiffFromUTC
                      isEveningPeak = SP.checkEveningPeakInWeekday createdAt minuteDiffFromUTC
                      isWeekendPeak = SP.checkWeekendPeak createdAt minuteDiffFromUTC
                  when isMorningPeak do QP.incrementMorningPeakRidesCount personId
                  when isEveningPeak do QP.incrementEveningPeakRidesCount personId
                  when isWeekendPeak do QP.incrementWeekendPeakRidesCount personId
                  let isAnyPeak = isMorningPeak || isEveningPeak || isWeekendPeak
                  unless isAnyPeak do QP.incrementOffpeakRidesCount personId
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
