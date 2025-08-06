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
import "beckn-spec" Domain.Types.RideStatus as DDR
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import "sessionizer-metrics" Lib.SessionizerMetrics.Types.Event as E
import "rider-app" SharedLogic.Person as SP
import "rider-app" Storage.CachedQueries.Merchant.RiderConfig as QRC
import "rider-app" Storage.Queries.BookingCancellationReason as QBCR
import "rider-app" Storage.Queries.PersonStats as QP
import "rider-app" Tools.Error
import qualified "rider-app" Tools.Event as TE

updateCustomerStats :: E.Event TE.Payload -> Text -> Flow ()
updateCustomerStats event _ = do
  whenJust event.personId $ \pId -> do
    when (event.service == RIDER_APP && (event.eventType == RideEnded || event.eventType == BookingCancelled)) do
      let personId = Id pId
          mbOperatingCityId = Id <$> event.merchantOperatingCityId
      case mbOperatingCityId of
        Nothing -> logInfo "No merchant operating city id found"
        Just merchantOperatingCityId -> do
          personStats <-
            runInReplica $
              QP.findByPersonId personId >>= \case
                Nothing -> do
                  logDebug $ "PersonStats not found for personId: " <> personId.getId
                  personData <- getBackfillPersonStatsData personId merchantOperatingCityId
                  QP.createPersonStats personData
                Just ps | isNotBackfilled ps -> do
                  personData <- getBackfillPersonStatsData personId merchantOperatingCityId
                  QP.incrementOrSetPersonStats personData
                  pure personData
                Just ps -> pure ps

          whenJust (event.payload) $ \payload -> do
            maxTimevalue :: (Maybe UTCTime) <- Hedis.get (personRedisKey personId)
            unless (isNotBackfilled personStats) do
              let eventCreatedAt = payload.cAt
              now <- getCurrentTime
              if isJust maxTimevalue && (diffUTCTime (fromMaybe now maxTimevalue) eventCreatedAt > 0)
                then do
                  logInfo $ "Event is older than the last event of the same type for personId: " <> personId.getId
                else do
                  case event.eventType of
                    RideEnded -> do
                      case payload.rs of
                        DDR.COMPLETED -> do
                          let createdAt = payload.cAt
                          riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
                          let minuteDiffFromUTC = (riderConfig.timeDiffFromUtc.getSeconds) `div` 60
                          -- Esq.runNoTransaction $ do
                          let ifIsWeekend = SP.isWeekend createdAt minuteDiffFromUTC
                              isMorningPeak = SP.checkMorningPeakInWeekday createdAt minuteDiffFromUTC
                              isEveningPeak = SP.checkEveningPeakInWeekday createdAt minuteDiffFromUTC
                              isWeekendPeak = SP.checkWeekendPeak createdAt minuteDiffFromUTC
                          let isAnyPeak = isMorningPeak || isEveningPeak || isWeekendPeak

                          -- here we will increment all these in one function
                          QP.incrementCompletedRidesEventCount personId ifIsWeekend isMorningPeak isEveningPeak isWeekendPeak isAnyPeak
                        _ -> pure ()
                    BookingCancelled -> do
                      let bookingId = cast payload.bId
                      -- bookingCancellationReason <- Esq.runInReplica $ QBCR.findByRideBookingId bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
                      bookingCancellationReason <- runInReplica $ QBCR.findByRideBookingId bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
                      -- Esq.runNoTransaction $ do
                      when (bookingCancellationReason.source == SBCR.ByUser) do QP.incrementUserCancelledRidesCount personId
                      when (bookingCancellationReason.source == SBCR.ByDriver) do QP.incrementDriverCancelledRidesCount personId
                    _ -> do
                      logError $ "Event type not handled for personId: " <> personId.getId <> " eventType: " <> show event.eventType
                      pure ()
  where
    isNotBackfilled personStats_ = all (== 0) [personStats_.userCancelledRides, personStats_.completedRides, personStats_.weekendRides, personStats_.weekdayRides, personStats_.offPeakRides, personStats_.eveningPeakRides, personStats_.morningPeakRides, personStats_.weekendPeakRides]
