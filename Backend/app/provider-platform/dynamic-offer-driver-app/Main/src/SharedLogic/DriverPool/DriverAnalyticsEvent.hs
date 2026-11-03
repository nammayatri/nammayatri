module SharedLogic.DriverPool.DriverAnalyticsEvent
  ( publishPopupsSent,
    publishPopupResponse,
    publishPopupAccepted,
    publishRideAssigned,
    publishRideStarted,
    publishRideCompleted,
    publishRideCancelled,
    publishRating,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as AT
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.Common (SearchRequestForDriverResponse (..))
import qualified Domain.Types.FareParameters as DFP
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequestForDriver as DSRD
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverPool.DriverPoolStateEvent (DriverPoolStateEnv)

-- | One thing a driver did, for the sessionizer's driver_analytics_5m rows:
-- the counts it adds (column -> amount; money as a decimal number) to the row
-- of the driver's state at ts. Keyed by driver id, like driver-pool-state and
-- location-updates, so one sessionizer pod sees all of a driver's records.
data DriverAnalyticsEvent = DriverAnalyticsEvent
  { driverId :: Text,
    -- | Epoch milliseconds when the event happened.
    ts :: Milliseconds,
    merchantOperatingCityId :: Text,
    counts :: A.Value
  }
  deriving (Generic, Show, ToJSON)

-- | Best-effort, like 'SharedLogic.DriverPool.DriverPoolStateEvent.publishDriverPoolData':
-- reads nothing, a failed send is logged and never reaches the caller, and no
-- topic configured sends nothing.
publish ::
  (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  [AT.Pair] ->
  m ()
publish dId cityId counts = do
  now <- getCurrentTime
  publishAt now dId cityId counts

publishAt ::
  (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) =>
  UTCTime ->
  Id Person ->
  Id MerchantOperatingCity ->
  [AT.Pair] ->
  m ()
publishAt eventTime dId cityId counts = do
  mbTopic <- asks (.driverAnalyticsEventsTopic)
  whenJust mbTopic $ \topic -> do
    let ev =
          DriverAnalyticsEvent
            { driverId = dId.getId,
              ts = Milliseconds (round (utcTimeToPOSIXSeconds eventTime * 1000)),
              merchantOperatingCityId = cityId.getId,
              counts = A.object counts
            }
    res <- withTryCatch "publishDriverAnalyticsEvent" $ produceMessage (topic, Just $ TE.encodeUtf8 dId.getId) ev
    case res of
      Left err -> logError $ "publishDriverAnalyticsEvent failed for driver " <> dId.getId <> ": " <> show err
      Right () -> pure ()

count :: Text -> Bool -> [AT.Pair]
count column c = [AK.fromText column A..= (1 :: Int) | c]

money :: Text -> Maybe HighPrecMoney -> [AT.Pair]
money column = maybe [] (\m -> [AK.fromText column A..= (fromRational m.getHighPrecMoney :: Double)])

publishPopupsSent :: (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) => [DSRD.SearchRequestForDriver] -> m ()
publishPopupsSent = mapM_ $ \srd -> publish srd.driverId srd.merchantOperatingCityId (count "popups_sent" True)

-- | A rejected or pulled pop-up, at the time of publishing.
publishPopupResponse ::
  (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) =>
  DSRD.SearchRequestForDriver ->
  SearchRequestForDriverResponse ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ()
publishPopupResponse srd response mbRenderedAt mbRespondedAt =
  publish srd.driverId srd.merchantOperatingCityId (popupCounts srd response mbRenderedAt mbRespondedAt)

-- | An accepted pop-up, stamped with the time just before the accept ran, so it
-- counts in the state the driver accepted from: the accept can assign the ride
-- and move them to onRide before this is published.
publishPopupAccepted ::
  (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) =>
  DSRD.SearchRequestForDriver ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ()
publishPopupAccepted srd acceptStartedAt mbRenderedAt mbRespondedAt =
  publishAt acceptStartedAt srd.driverId srd.merchantOperatingCityId (popupCounts srd Accept mbRenderedAt mbRespondedAt)

-- | response_secs_sum counts only answers with a valid rendered/responded pair.
popupCounts :: DSRD.SearchRequestForDriver -> SearchRequestForDriverResponse -> Maybe UTCTime -> Maybe UTCTime -> [AT.Pair]
popupCounts srd response mbRenderedAt mbRespondedAt =
  count "popups_accepted" (response == Accept)
    <> count "popups_rejected" (response == Reject)
    <> count "popups_pulled" (response == Pulled)
    <> count "auto_accept_search_requests" (response == Accept && srd.isAutoAccepted == Just True)
    <> [ "response_secs_sum" A..= secs
         | response /= Pulled,
           Just rendered <- [mbRenderedAt],
           Just responded <- [mbRespondedAt],
           let secs = (floor (diffUTCTime responded rendered) :: Integer),
           secs > 0
       ]

publishRideAssigned :: (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) => DBooking.Booking -> DRide.Ride -> m ()
publishRideAssigned booking ride =
  publish ride.driverId ride.merchantOperatingCityId $
    count "total_bookings" True
      <> count "auto_accept_bookings" (autoAccepted booking)
      <> money "driver_addition" (DFP.driverSelectedFare booking.fareParams)
      <> money "customer_tip" (DFP.customerExtraFee booking.fareParams)

publishRideStarted :: (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) => DRide.Ride -> m ()
publishRideStarted ride = publish ride.driverId ride.merchantOperatingCityId (count "rides_started" True)

-- | The ended ride, so fare is the final fare.
publishRideCompleted :: (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) => DBooking.Booking -> DRide.Ride -> m ()
publishRideCompleted booking ride =
  publish ride.driverId ride.merchantOperatingCityId $
    count "finished_rides" True
      <> count "auto_accept_finished_rides" (autoAccepted booking)
      <> money "gross_earnings" ride.fare

publishRideCancelled :: (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) => DBooking.Booking -> DRide.Ride -> DBCR.CancellationSource -> m ()
publishRideCancelled booking ride source =
  publish ride.driverId ride.merchantOperatingCityId $
    count "cancelled_rides" True
      <> count "rides_cancelled_by_driver" byDriver
      <> count "rides_cancelled_by_user" byUser
      <> count "auto_accept_cancelled_rides" auto
      <> count "auto_accept_rides_cancelled_by_driver" (auto && byDriver)
      <> count "auto_accept_rides_cancelled_by_user" (auto && byUser)
  where
    byDriver = source == DBCR.ByDriver
    byUser = source == DBCR.ByUser
    auto = autoAccepted booking

-- | A new rating counts the ride and its value; a changed one only the
-- difference, so rating_sum stays the sum of current ratings.
publishRating :: (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) => DRide.Ride -> Bool -> Int -> m ()
publishRating ride isNew ratingDelta =
  publish ride.driverId ride.merchantOperatingCityId $
    count "rides_with_rating" isNew <> ["rating_sum" A..= ratingDelta | ratingDelta /= 0]

autoAccepted :: DBooking.Booking -> Bool
autoAccepted booking = booking.isAutoAccepted == Just True
