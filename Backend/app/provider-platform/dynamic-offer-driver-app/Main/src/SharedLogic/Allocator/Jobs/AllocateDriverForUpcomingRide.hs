module SharedLogic.Allocator.Jobs.AllocateDriverForUpcomingRide where

import qualified Control.Monad.Catch as Exception
import Data.Time.LocalTime
import qualified Database.Redis as Hedis
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.Booking.BookingLocation as Booking
import qualified Domain.Types.DriverQuote as DDriverQuote
import qualified Domain.Types.FarePolicy.FarePolicy as FarePolicy
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSearchReq
import qualified Domain.Types.Timetable as Timetable
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), SendSearchRequestToDriverJobData (..))
import SharedLogic.FareCalculator (calculateFare)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.AllocatorJob as QAllocatorJob
import qualified Storage.Queries.DriverQuote as QDriverQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Storage.Queries.Timetable as QTimetable
import Tools.Maps as Maps

withoutDuplicateExecution ::
  (MonadMask m, Redis.HedisFlow m r) =>
  Text ->
  Redis.ExpirationTime ->
  m ExecutionResult ->
  m ExecutionResult
withoutDuplicateExecution key timeout action =
  bracket
    (Redis.tryLockRedis key timeout)
    (bool (Redis.unlockRedis key) (pure ()))
    (bool action (pure DuplicateExecution))

data NotImplemented = NotImplemented
  deriving (Show, Exception, IsBaseError)

data BookingNotFound = BookingNotFound
  deriving (Show, Exception, IsBaseError)

data QuoteError = QuoteError
  deriving (Show, Exception, IsBaseError)

handle ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    MonadCatch m
  ) =>
  Job 'AllocateDriverForUpcomingRide ->
  m ExecutionResult
handle (Job {jobData}) =
  Exception.handle (\(SomeException e) -> pure $ Terminate (show e)) $
    withoutDuplicateExecution lockKey timeout $ do
      upcomingBooking <- fromMaybeM BookingNotFound =<< QTimetable.findUpcomingBooking jobData.timetableId
      searchRequest <- createSearchForUpcomingBooking upcomingBooking
      quote <- findQuoteForSearchRequest (Id upcomingBooking.providerId) upcomingBooking.farePolicy searchRequest
      booking <- createBooking upcomingBooking searchRequest quote
      Complete <$ notifyScheduledRide booking
  where
    notifyScheduledRide _booking =
      -- throwM NotImplemented
      pure ()

    timeout =
      60

    lockKey =
      "lock:AllocateDriverForUpcomingRide:" <> getId jobData.timetableId

createSearchForUpcomingBooking ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  Timetable.UpcomingBooking ->
  m DSearchReq.SearchRequest
createSearchForUpcomingBooking upcomingBooking = do
  let merchantId = Id upcomingBooking.providerId
  (distance, duration) <- do
    res <-
      Maps.getDistance merchantId $
        Maps.GetDistanceReq
          { origin = upcomingBooking.fromLocation,
            destination = upcomingBooking.toLocation,
            travelMode = Just Maps.CAR
          }
    pure (res.distance, res.duration)
  timeZone <- liftIO getCurrentTimeZone
  let pickupTime = localTimeToUTC timeZone upcomingBooking.pickupTime
  now <- getCurrentTime
  id_ <- generateGUID
  fromLocation <- searchRequestLocationFromBookingLocation now upcomingBooking.fromLocation
  toLocation <- searchRequestLocationFromBookingLocation now upcomingBooking.toLocation
  let validTill_ = 60 `addUTCTime` now
      sReq =
        DSearchReq.SearchRequest
          { id = id_,
            transactionId = getId upcomingBooking.recurringBookingId,
            messageId = getId upcomingBooking.id,
            startTime = pickupTime,
            validTill = validTill_,
            providerId = merchantId,
            fromLocation = fromLocation,
            toLocation = toLocation,
            bapId = upcomingBooking.bapId,
            bapUri = upcomingBooking.bapUri,
            estimatedDistance = distance,
            estimatedDuration = duration,
            createdAt = now,
            vehicleVariant = upcomingBooking.farePolicy.vehicleVariant,
            status = DSearchReq.ACTIVE,
            updatedAt = now,
            autoAssignEnabled = True,
            automatedSearch = True
          }
  Esq.runTransaction $ QSearchReq.create sReq
  pure sReq

searchRequestLocationFromBookingLocation ::
  MonadGuid m =>
  UTCTime ->
  Booking.BookingLocation ->
  m DSearchReq.SearchReqLocation
searchRequestLocationFromBookingLocation now location = do
  id <- generateGUID
  pure $
    DSearchReq.SearchReqLocation
      { id = id,
        lat = location.lat,
        lon = location.lon,
        street = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        building = Nothing,
        areaCode = Nothing,
        area = Nothing,
        full_address = Nothing,
        createdAt = now,
        updatedAt = now
      }

findQuoteForSearchRequest ::
  (Redis.HedisFlow m r, EsqDBFlow m r, MonadThrow m) =>
  Id DMerchant.Merchant ->
  FarePolicy.FarePolicy ->
  DSearchReq.SearchRequest ->
  m DDriverQuote.DriverQuote
findQuoteForSearchRequest merchantId farePolicy searchRequest = do
  let distance = searchRequest.estimatedDistance
  fareParams <- calculateFare merchantId farePolicy distance searchRequest.startTime Nothing
  let driverExtraFare = farePolicy.driverExtraFee
  Esq.runTransaction $
    QAllocatorJob.createAllocatorSendSearchRequestToDriverJob 0 $
      SendSearchRequestToDriverJobData
        { requestId = searchRequest.id,
          baseFare = fareParams.baseFare,
          estimatedRideDistance = distance,
          driverMinExtraFee = driverExtraFare.minFee,
          driverMaxExtraFee = driverExtraFare.maxFee
        }
  fromMaybeM QuoteError =<< getFirstQuote
  where
    -- Wait for first driver to respond with a quote by blocking on a redis stream
    -- we will wait up to 30 seconds for driver to respond.
    getFirstQuote = do
      key <- Redis.buildKey $ "searchRequest:" <> getId searchRequest.id <> ":quotes"
      let thirtySeconds = 30000
      records <-
        Redis.runHedis $
          Hedis.xreadOpts [(key, "0-0")] $
            Hedis.XReadOpts
              { block = Just thirtySeconds,
                recordCount = Just 1
              }
      let mDriverQuoteId =
            case records of
              Just [xreadResponse] ->
                case Hedis.records xreadResponse of
                  [xreadResponseRecord] ->
                    fmap (Id . decodeUtf8) $ lookup "driverQuoteId" $ Hedis.keyValues xreadResponseRecord
                  _ ->
                    Nothing
              _ -> Nothing
      case mDriverQuoteId of
        Just driverQuoteId ->
          QDriverQuote.findById driverQuoteId
        Nothing ->
          pure Nothing

createBooking ::
  (MonadTime m, MonadGuid m, EsqDBFlow m r, MonadThrow m) =>
  Timetable.UpcomingBooking ->
  DSearchReq.SearchRequest ->
  DDriverQuote.DriverQuote ->
  m Booking.SimpleBooking
createBooking upcomingBooking searchRequest quote = do
  now <- getCurrentTime
  bookingId <- generateGUID
  let booking =
        Booking.SimpleBooking
          { id = bookingId,
            quoteId = quote.id,
            status = Booking.NEW,
            providerId = searchRequest.providerId,
            bapId = upcomingBooking.bapId,
            bapUri = upcomingBooking.bapUri,
            startTime = searchRequest.startTime,
            riderId = Nothing,
            riderName = Nothing,
            fromLocation = upcomingBooking.fromLocation.id,
            toLocation = upcomingBooking.toLocation.id,
            vehicleVariant = quote.vehicleVariant,
            estimatedDistance = quote.distance,
            estimatedFare = quote.estimatedFare,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParamsId = quote.fareParams.id,
            createdAt = now,
            updatedAt = now
          }
  Esq.runTransaction $ do
    Esq.create booking
    QTimetable.updateTimetableWithBookingId upcomingBooking.id bookingId
  pure booking
