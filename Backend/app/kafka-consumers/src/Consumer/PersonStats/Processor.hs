{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.PersonStats.Processor
  ( updatePersonStats,
  )
where

import Data.Time
import "rider-app" Domain.Action.UI.CleverTap as DC
import "rider-app" Domain.Types.BookingCancellationReason as SBCR
import "dynamic-offer-driver-app" Domain.Types.Ride as DDR
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import "sessionizer-metrics" Lib.SessionizerMetrics.Types.Event as E
import "rider-app" Storage.Queries.BookingCancellationReason as QBCR
import "rider-app" Storage.Queries.Person.PersonStats as QP
import "rider-app" Storage.Queries.Ride as QR
import "rider-app" Tools.Error
import "dynamic-offer-driver-app" Tools.Event as TE

updatePersonStats :: E.Event TE.Payload -> Text -> Flow ()
updatePersonStats event _ = do
  when (isJust event.personId && event.service == RIDER_APP) do
    let personId = Id (fromMaybe "" event.personId)

    when (event.eventType == RideEnded) $ do
      whenJust (event.payload) $ \payload -> do
        when (payload.rs == DDR.COMPLETED) do
          let createdAt = payload.cAt

          personStats_ <- Esq.runInReplica $ QP.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
          when (all (== 0) [personStats_.userCancelledRides, personStats_.completedRides, personStats_.weekendRides, personStats_.weekdayRides, personStats_.offPeakRides, personStats_.eveningPeakRides, personStats_.morningPeakRides, personStats_.weekendPeakRides]) $ do
            personStatsValues <- DC.backfillPersonStats personId Nothing
            Esq.runNoTransaction $ QP.incrementOrSetPersonStats personStatsValues

          Esq.runNoTransaction $ do
            when (DC.isWeekend createdAt IST) do QP.incrementWeekendRidesCount personId
            unless (DC.isWeekend createdAt IST) do QP.incrementWeekdayRidesCount personId
            when (checkMorningPeakInWeekdays createdAt IST) do QP.incrementMorningPeakRidesCount personId
            when (checkEveningPeakInWeekdays createdAt IST) do QP.incrementEveningPeakRidesCount personId
            when (checkWeekendPeaks createdAt IST) do QP.incrementWeekendPeakRidesCount personId
            when (not (checkEveningPeakInWeekdays createdAt IST) && not (checkMorningPeakInWeekdays createdAt IST) && not (checkWeekendPeaks createdAt IST)) do QP.incrementOffpeakRidesCount personId
            QP.incrementCompletedRidesCount personId

    when (event.eventType == BookingCancelled && isJust (event.payload)) $ do
      whenJust (event.payload) $ \payload -> do
        let rideId = cast payload.rId
        ride <- Esq.runInReplica $ QR.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
        bookingCancellationReason <- Esq.runInReplica $ QBCR.findByRideBookingId ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
        if bookingCancellationReason.source == SBCR.ByUser
          then Esq.runNoTransaction $ QP.incrementUserCancelledRidesCount personId
          else Esq.runNoTransaction $ QP.incrementDriverCancelledRidesCount personId

checkMorningPeakInWeekdays :: UTCTime -> LocalZone -> Bool
checkMorningPeakInWeekdays utcTime locZone = not (DC.isWeekend utcTime locZone) && DC.within (convertTimeZone utcTime locZone) (TimeOfDay 8 0 0) (TimeOfDay 11 0 0)

checkEveningPeakInWeekdays :: UTCTime -> LocalZone -> Bool
checkEveningPeakInWeekdays utcTime locZone = not (DC.isWeekend utcTime locZone) && DC.within (convertTimeZone utcTime locZone) (TimeOfDay 16 0 0) (TimeOfDay 19 0 0)

checkWeekendPeaks :: UTCTime -> LocalZone -> Bool
checkWeekendPeaks utcTime locZone = DC.isWeekend utcTime locZone && DC.within (convertTimeZone utcTime locZone) (TimeOfDay 12 30 0) (TimeOfDay 19 30 0)
