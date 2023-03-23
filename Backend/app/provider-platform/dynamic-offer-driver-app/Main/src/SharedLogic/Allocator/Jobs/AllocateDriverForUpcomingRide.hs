module SharedLogic.Allocator.Jobs.AllocateDriverForUpcomingRide where

import qualified Beckn.ACL.OnInit as ACL
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Beckn.Types.Core.Taxi.OnInit.Fulfillment as OnInit
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as BS
import Data.Time.LocalTime
import qualified Database.Redis as Hedis
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.Booking.BookingLocation as Booking
import qualified Domain.Types.DriverQuote as DDriverQuote
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FarePolicy as FarePolicy
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSearchReq
import qualified Domain.Types.Timetable as Timetable
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth (getHttpManagerKey)
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), SendSearchRequestToDriverJobData (..))
import qualified SharedLogic.CallBAP as CallBAP
import SharedLogic.FareCalculator (calculateFare)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import Storage.CachedQueries.Exophone (findRandomExophone)
import qualified Storage.CachedQueries.Merchant as QMerchant
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

data MerchantNotFound = MerchantNotFound
  deriving (Show, Exception, IsBaseError)

data ExophoneNotFound = ExophoneNotFound Text
  deriving (Show, Exception, IsBaseError)

data BookingNotFound = BookingNotFound
  deriving (Show, Exception, IsBaseError)

data QuoteError = QuoteError
  deriving (Show, Exception, IsBaseError)

handle ::
  ( EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "maxShards" ::: Int],
    HasShortDurationRetryCfg r c,
    HasHttpClientOptions r c,
    CacheFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    MonadCatch m
  ) =>
  Job 'AllocateDriverForUpcomingRide ->
  m ExecutionResult
handle job =
  Exception.handle (\(SomeException e) -> pure $ Terminate (show e)) $
    withoutDuplicateExecution lockKey timeout $ do
      upcomingBooking <- fromMaybeM BookingNotFound =<< QTimetable.findUpcomingBooking jobData.timetableId
      transporter <- fromMaybeM MerchantNotFound =<< QMerchant.findById (Id upcomingBooking.providerId)
      searchRequest <- createSearchForUpcomingBooking upcomingBooking
      quote <- findQuoteForSearchRequest transporter.id upcomingBooking.farePolicy searchRequest
      booking <- createBooking upcomingBooking transporter searchRequest quote
      Complete <$ notifyScheduledRide upcomingBooking transporter quote booking
  where
    jobData =
      job.jobInfo.jobData

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
            automatedSearch = True,
            searchRepeatCounter = 0
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
  (Redis.HedisFlow m r, EsqDBFlow m r, MonadThrow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl, "maxShards" ::: Int]) =>
  Id DMerchant.Merchant ->
  FarePolicy.FarePolicy ->
  DSearchReq.SearchRequest ->
  m DDriverQuote.DriverQuote
findQuoteForSearchRequest merchantId farePolicy searchRequest = do
  let distance = searchRequest.estimatedDistance
  fareParams <- calculateFare merchantId farePolicy distance searchRequest.startTime Nothing
  let driverExtraFare = farePolicy.driverExtraFee
  maxShards <- asks (.maxShards)
  Esq.runTransaction $
    QAllocatorJob.createAllocatorSendSearchRequestToDriverJob 0 maxShards $
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
      mDriverQuoteId <- listenForSearchRequestQuote searchRequest.id 30
      case mDriverQuoteId of
        Just driverQuoteId ->
          QDriverQuote.findById driverQuoteId
        Nothing ->
          pure Nothing

buildSearchRequestKey :: Redis.HedisFlow m r => Id DSearchReq.SearchRequest -> m BS.ByteString
buildSearchRequestKey searchRequestId =
  Redis.buildKey $ "searchRequest:" <> getId searchRequestId <> ":quotes"

notifySearchRequestQuote ::
  Redis.HedisFlow m r =>
  Id DSearchReq.SearchRequest ->
  Id DDriverQuote.DriverQuote ->
  m ()
notifySearchRequestQuote searchRequestId driverQuoteId = do
  let timeoutInSeconds = 30 -- Since we are listening in the running job 30 seconds should be more than enough time
  key <- buildSearchRequestKey searchRequestId
  void $
    Redis.runHedis $ do
      void $ Hedis.lpush key [encodeUtf8 $ getId driverQuoteId]
      Hedis.expire key timeoutInSeconds

listenForSearchRequestQuote :: Redis.HedisFlow m r => Id DSearchReq.SearchRequest -> Integer -> m (Maybe (Id DDriverQuote.DriverQuote))
listenForSearchRequestQuote searchRequestId timeoutInSeconds = do
  key <- buildSearchRequestKey searchRequestId
  record <- Redis.runHedis $ Hedis.brpop [key] timeoutInSeconds
  pure $ fmap (Id . decodeUtf8 . snd) record

createBooking ::
  (MonadTime m, MonadGuid m, EsqDBFlow m r, MonadThrow m, CacheFlow m r) =>
  Timetable.UpcomingBooking ->
  DMerchant.Merchant ->
  DSearchReq.SearchRequest ->
  DDriverQuote.DriverQuote ->
  m Booking.SimpleBooking
createBooking upcomingBooking transporter searchRequest quote = do
  now <- getCurrentTime
  bookingId <- generateGUID
  exophone <-
    findRandomExophone transporter.id
      >>= fromMaybeM (ExophoneNotFound transporter.id.getId)
  let booking =
        Booking.SimpleBooking
          { id = bookingId,
            transactionId = searchRequest.transactionId,
            bookingType = Booking.NormalBooking,
            specialZoneOtpCode = Nothing,
            primaryExophone = DExophone.getPhone exophone,
            quoteId = quote.id.getId,
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

notifyScheduledRide ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c
  ) =>
  Timetable.UpcomingBooking ->
  DMerchant.Merchant ->
  DDriverQuote.DriverQuote ->
  Booking.SimpleBooking ->
  m ()
notifyScheduledRide upcomingBooking transporter quote booking = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- CallBAP.buildBppUrl transporter.id
  context <-
    buildTaxiContext
      Context.ON_INIT
      (getId booking.id)
      Nothing
      booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) transporter.city
  let scheduledRideFulfillment =
        OnInit.Fulfillment
          { id = upcomingBooking.recurringBookingId.getId,
            _type = Just "SCHEDULED_RIDE",
            start = Just (OnInit.FulfillmentDetail $ OnInit.TimeTimestamp booking.startTime),
            end = Nothing
          }

  void $
    withShortRetry $
      Beckn.callBecknAPI (Just authKey) Nothing (show Context.INIT) OnInit.onInitAPI booking.bapUri $
        BecknCallbackReq context $
          Right $
            ACL.mkOnInitMessage' booking.id.getId quote.estimatedFare quote.fareParams (Just scheduledRideFulfillment)
