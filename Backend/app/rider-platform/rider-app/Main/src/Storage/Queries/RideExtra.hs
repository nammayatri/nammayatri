{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RideExtra where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Ride as Common
import Data.List.Extra (notNull)
import qualified Data.Text.Encoding as TE
import Data.Time hiding (getCurrentTime)
import qualified Database.Beam as B
import Database.Beam.Backend (autoSqlValueSyntax)
import qualified Database.Beam.Backend as BeamBackend
import Domain.Types
import Domain.Types.Booking as Booking
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Client as DC
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride as Ride
import qualified EulerHS.Language as L
import EulerHS.Prelude (ByteString, forM_, whenNothingM_)
import EulerHS.Types (KVDBAnswer)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude hiding (forM_)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common (distanceToHighPrecDistance, distanceToHighPrecMeters)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, logTagDebug, logTagError)
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Ride as BeamR
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.Ride ()
import Storage.Queries.Person ()
import Tools.Metrics (CoreMetrics)

createRide' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "storeRidesTimeLimit" r Int) => Ride -> m ()
createRide' ride = createWithKV ride >> appendByDriverPhoneNumber ride

createRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "storeRidesTimeLimit" r Int) => Ride -> m ()
createRide ride = do
  processSingleLocation ride.fromLocation SLM.buildPickUpLocationMapping
  when (notNull ride.stops) $ processMultipleLocations ride.stops
  whenJust ride.toLocation $ \toLocation -> processSingleLocation toLocation SLM.buildDropLocationMapping
  createRide' ride
  where
    processSingleLocation location locationMappingCreator = do
      locationMap <- locationMappingCreator location.id ride.id.getId DLM.RIDE ride.merchantId ride.merchantOperatingCityId
      QLM.create locationMap
      whenNothingM_ (QL.findById location.id) $ do QL.create location

    processMultipleLocations locations = do
      locationMappings <- SLM.buildStopsLocationMapping locations ride.id.getId DLM.RIDE ride.merchantId ride.merchantOperatingCityId
      QLM.createMany locationMappings
      locations `forM_` \location ->
        whenNothingM_ (QL.findById location.id) $ do QL.create location

data DatabaseWith3 table1 table2 table3 f = DatabaseWith3
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2),
    dwTable3 :: f (B.TableEntity table3)
  }
  deriving (Generic, B.Database be)

updateStatus :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> RideStatus -> m ()
updateStatus rideId status = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.status status,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateTrackingUrl :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> BaseUrl -> m ()
updateTrackingUrl rideId url = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.trackingUrl (Just $ showBaseUrl url),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateRideRating :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> Int -> Maybe Bool -> m ()
updateRideRating rideId rideRating wasRideSafe = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.rideRating (Just rideRating),
      Se.Set BeamR.wasRideSafe wasRideSafe,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateMultiple :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> Ride -> m ()
updateMultiple rideId ride = do
  let distanceUnit = ride.distanceUnit
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.status ride.status,
      Se.Set BeamR.fare (ride.fare <&> (.amount)),
      Se.Set BeamR.totalFare (ride.totalFare <&> (.amount)),
      Se.Set BeamR.currency (ride.fare <&> (.currency)),
      Se.Set BeamR.chargeableDistance (distanceToHighPrecMeters <$> ride.chargeableDistance),
      Se.Set BeamR.chargeableDistanceValue $ distanceToHighPrecDistance distanceUnit <$> ride.chargeableDistance,
      Se.Set BeamR.traveledDistance (distanceToHighPrecMeters <$> ride.traveledDistance),
      Se.Set BeamR.traveledDistanceValue $ distanceToHighPrecDistance distanceUnit <$> ride.traveledDistance,
      Se.Set BeamR.distanceUnit $ Just distanceUnit,
      Se.Set BeamR.rideStartTime ride.rideStartTime,
      Se.Set BeamR.rideEndTime ride.rideEndTime,
      Se.Set BeamR.endOtp ride.endOtp,
      Se.Set BeamR.startOdometerReading ride.startOdometerReading,
      Se.Set BeamR.endOdometerReading ride.endOdometerReading,
      Se.Set BeamR.tollConfidence ride.tollConfidence,
      Se.Set BeamR.estimatedEndTimeRangeStart ((.start) <$> ride.estimatedEndTimeRange),
      Se.Set BeamR.estimatedEndTimeRangeEnd ((.end) <$> ride.estimatedEndTimeRange),
      Se.Set BeamR.paymentStatus (Just ride.paymentStatus),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

findActiveByRBId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m (Maybe Ride)
findActiveByRBId (Id rbId) = findOneWithKV [Se.And [Se.Is BeamR.bookingId $ Se.Eq rbId, Se.Is BeamR.status $ Se.Not $ Se.Eq Ride.CANCELLED]]

findLatestCompletedRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Ride)
findLatestCompletedRide riderId = do
  booking <- findAllWithOptionsKV [Se.Is BeamB.riderId $ Se.Eq $ getId riderId] (Se.Desc BeamB.createdAt) Nothing Nothing
  findAllWithOptionsKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId <$> (DRB.id <$> booking), Se.Is BeamR.status $ Se.Eq Ride.COMPLETED]] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

updateDriverArrival :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> Maybe UTCTime -> m ()
updateDriverArrival rideId arrivalTime = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.driverArrivalTime arrivalTime,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateSafetyCheckStatus :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> Maybe Bool -> m ()
updateSafetyCheckStatus rideId status = do
  updateOneWithKV
    [ Se.Set BeamR.safetyCheckStatus status
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    riderId :: Id Person
  }

findStuckRideItems :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> [Id Booking] -> UTCTime -> m [StuckRideItem]
findStuckRideItems merchant moCity bookingIds now = do
  let now6HrBefore = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
  bookings <-
    findAllWithDb
      [ Se.And
          [ Se.Is BeamB.providerId $ Se.Eq merchant.id.getId,
            Se.Is BeamB.id $ Se.In $ getId <$> bookingIds,
            Se.Is BeamB.fareProductType $ Se.Not $ Se.In [DQuote.RENTAL, DQuote.INTER_CITY],
            Se.Or
              ( [Se.Is BeamB.merchantOperatingCityId $ Se.Eq (Just $ getId moCity.id)]
                  <> [Se.Is BeamB.merchantOperatingCityId $ Se.Eq Nothing | merchant.defaultCity == moCity.city]
              )
          ]
      ]
  rides <-
    findAllWithDb
      [ Se.And
          [ Se.Is BeamR.status $ Se.Eq Ride.NEW,
            Se.Is BeamR.createdAt $ Se.LessThanOrEq now6HrBefore,
            Se.Is BeamR.bookingId $ Se.In $ getId . DRB.id <$> bookings
          ]
      ]

  let rideBooking = foldl' (getRideWithBooking bookings) [] rides
  pure $ mkStuckRideItem <$> rideBooking
  where
    getRideWithBooking bookings acc ride' =
      let bookings' = filter (\x -> x.id == ride'.bookingId) bookings
       in acc <> ((\x -> (ride'.id, x.id, x.riderId)) <$> bookings')

    mkStuckRideItem (rideId, bookingId, riderId) = StuckRideItem {..}

cancelRides :: (MonadFlow m, EsqDBFlow m r) => [Id Ride] -> UTCTime -> m ()
cancelRides rideIds now =
  updateWithKV
    [ Se.Set BeamR.status Ride.CANCELLED,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.In $ getId <$> rideIds)]

data RideItem = RideItem
  { person :: Person,
    ride :: Ride,
    bookingStatus :: Common.BookingStatus,
    bookingDetails :: BookingDetails,
    rideScheduledAt :: UTCTime,
    tripCategory :: Maybe TripCategory
  }

instance BeamBackend.BeamSqlBackend be => B.HasSqlEqualityCheck be Common.BookingStatus

instance BeamBackend.HasSqlValueSyntax be String => BeamBackend.HasSqlValueSyntax be Common.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

roundToMidnightUTC :: UTCTime -> UTCTime
roundToMidnightUTC (UTCTime day _) = UTCTime day 0

roundToMidnightUTCToDate :: UTCTime -> UTCTime
roundToMidnightUTCToDate (UTCTime day _) = UTCTime (addDays 1 day) 0

findAllRideItems ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  UTCTime ->
  m [RideItem]
findAllRideItems merchantID limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhone mbFrom mbTo now = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.filter_'
              ( \(booking, ride, person) ->
                  booking.merchantId B.==?. B.val_ (getId merchantID)
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rideShortId -> ride.shortId B.==?. B.val_ (getShortId rideShortId)) mbRideShortId
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> person.mobileNumberHash B.==?. B.val_ (Just hash)) mbCustomerPhoneDBHash
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverMobileNumber -> ride.driverMobileNumber B.==?. B.val_ driverMobileNumber) mbDriverPhone
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultFrom -> B.sqlBool_ $ ride.createdAt B.>=. B.val_ (roundToMidnightUTC defaultFrom)) mbFrom
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultTo -> B.sqlBool_ $ ride.createdAt B.<=. B.val_ (roundToMidnightUTCToDate defaultTo)) mbTo
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\bookingStatus -> mkBookingStatusVal ride B.==?. B.val_ bookingStatus) mbBookingStatus
              )
              do
                booking' <- B.all_ (BeamCommon.booking BeamCommon.atlasDB)
                ride' <- B.join_' (BeamCommon.ride BeamCommon.atlasDB) (\ride'' -> BeamR.bookingId ride'' B.==?. BeamB.id booking')
                person' <- B.join_' (BeamCommon.person BeamCommon.atlasDB) (\person'' -> BeamP.id person'' B.==?. BeamB.riderId booking')
                pure (booking', ride', person')
  res' <- case res of
    Right x -> do
      let bookings = fst' <$> x
          rides = snd' <$> x
          persons = thd' <$> x
      b <- catMaybes <$> mapM fromTType' bookings
      r <- catMaybes <$> mapM fromTType' rides
      p <- catMaybes <$> mapM fromTType' persons
      pure $ zip3 p r b
    Left _ -> pure []
  pure $ mkRideItem <$> res'
  where
    mkBookingStatusVal ride =
      B.ifThenElse_ (ride.status B.==. B.val_ Ride.COMPLETED) (B.val_ Common.RCOMPLETED) $
        B.ifThenElse_ (ride.status B.==. B.val_ Ride.NEW B.&&. B.not_ (ride.createdAt B.<=. B.val_ (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.UPCOMING) $
          B.ifThenElse_ (ride.status B.==. B.val_ Ride.NEW B.&&. (ride.createdAt B.<=. B.val_ (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.UPCOMING_6HRS) $
            B.ifThenElse_ (ride.status B.==. B.val_ Ride.INPROGRESS B.&&. B.not_ (ride.rideStartTime B.<=. B.val_ (Just $ addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.ONGOING) $
              B.ifThenElse_ (ride.status B.==. B.val_ Ride.CANCELLED) (B.val_ Common.RCANCELLED) (B.val_ Common.ONGOING_6HRS)
    mkBookingStatus now' ride
      | ride.status == Ride.COMPLETED = Common.RCOMPLETED
      | ride.status == Ride.NEW && ((ride.createdAt) > addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now') = Common.UPCOMING
      | ride.status == Ride.NEW && ride.createdAt <= addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now' = Common.UPCOMING_6HRS
      | ride.status == Ride.INPROGRESS && ((ride.rideStartTime) > Just (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now')) = Common.ONGOING
      | ride.status == Ride.CANCELLED = Common.RCANCELLED
      | otherwise = Common.ONGOING_6HRS
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    thd' (_, _, z) = z
    mkRideItem (person, ride, booking) =
      RideItem
        { bookingStatus = mkBookingStatus now ride,
          rideScheduledAt = booking.startTime,
          bookingDetails = booking.bookingDetails,
          tripCategory = booking.tripCategory,
          ..
        }

findRiderIdByRideId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ride -> m (Maybe (Id Person))
findRiderIdByRideId rideId = do
  ride <- findOneWithKV [Se.Is BeamR.id $ Se.Eq $ getId rideId]
  booking <- maybe (pure Nothing) (\ride' -> findOneWithKV [Se.Is BeamB.id $ Se.Eq $ getId (Ride.bookingId ride')]) ride
  pure $ Booking.riderId <$> booking

findAllByRiderIdAndRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> Maybe (Id DC.Client) -> Maybe UTCTime -> Maybe UTCTime -> [BookingStatus] -> m ([Booking], [Booking])
findAllByRiderIdAndRide (Id personId) mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate mbToDate mbBookingStatusList = do
  let isOnlyActive = Just True == mbOnlyActive
  let limit' = maybe 10 fromIntegral mbLimit
  let offset' = maybe 0 fromIntegral mbOffset
  bookings' <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is BeamB.riderId $ Se.Eq personId]
              <> ([Se.Is BeamB.status $ Se.Not $ Se.In [DRB.COMPLETED, DRB.CANCELLED, DRB.REALLOCATED] | isOnlyActive])
              <> ([Se.Is BeamB.status $ Se.Eq (fromJust mbBookingStatus) | isJust mbBookingStatus])
              <> ([Se.Is BeamB.clientId $ Se.Eq (getId <$> mbClientId) | isJust mbClientId])
              <> ([Se.Is BeamB.createdAt $ Se.GreaterThanOrEq (fromJust mbFromDate) | isJust mbFromDate])
              <> ([Se.Is BeamB.createdAt $ Se.LessThanOrEq (fromJust mbToDate) | isJust mbToDate])
              <> ([Se.Is BeamB.status $ Se.In mbBookingStatusList | not (null mbBookingStatusList)])
              <> ([Se.Is BeamB.journeyId $ Se.Eq Nothing])
          )
      ]
      (if isOnlyActive then Se.Asc BeamB.startTime else Se.Desc BeamB.startTime)
      (Just limit')
      (Just offset')
  otherActivePartyBooking <-
    if isOnlyActive && null bookings'
      then do
        bookingPartyLink <- QBPL.findOneActivePartyByRiderId (Id personId)
        case bookingPartyLink of
          Just bpl -> do
            booking <- maybeToList <$> QB.findById bpl.bookingId
            pure $
              filter
                ( \bk ->
                    (isNothing mbBookingStatus || Just (bk.status) == mbBookingStatus) && (isNothing mbClientId || bk.clientId == mbClientId) && (isNothing mbFromDate || isNothing mbToDate || (fromJust mbFromDate <= bk.createdAt && bk.createdAt <= fromJust mbToDate))
                )
                booking
          Nothing -> pure []
      else pure []
  let bookings = bookings' <> otherActivePartyBooking
  rides <- findAllWithOptionsKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId . DRB.id <$> bookings]] (Se.Desc BeamR.createdAt) Nothing Nothing
  let filteredBookings = matchBookingsWithRides bookings rides
  let filteredB = filterBookingsWithConditions filteredBookings
  pure (take limit' filteredB, bookings')
  where
    matchBookingsWithRides :: [Booking] -> [Ride.Ride] -> [(Booking, Maybe Ride.Ride)]
    matchBookingsWithRides bookings rides =
      [(booking, lookupRide booking rides) | booking <- bookings]
      where
        lookupRide :: Booking -> [Ride.Ride] -> Maybe Ride.Ride
        lookupRide booking = foldr (\ride acc -> if booking.id == ride.bookingId then Just ride else acc) Nothing
    filterBookingsWithConditions :: [(Booking, Maybe Ride.Ride)] -> [Booking]
    filterBookingsWithConditions filteredBookings =
      map fst $ filter (uncurry isBookingValid) filteredBookings
      where
        isBookingValid :: Booking -> Maybe Ride.Ride -> Bool
        isBookingValid booking maybeRide =
          let bookingDetails = DRB.bookingDetails booking
              otpCode =
                case bookingDetails of
                  DRB.OneWaySpecialZoneDetails details -> details.otpCode
                  _ -> Nothing
              isRentalOrInterCity = case bookingDetails of
                DRB.RentalDetails _ -> True
                DRB.InterCityDetails _ -> True
                _ -> False
           in isJust maybeRide || isJust otpCode || isRentalOrInterCity

findAllByRiderIdAndDriverNumber :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> Maybe (Id DC.Client) -> DbHash -> m [Ride]
findAllByRiderIdAndDriverNumber (Id personId) mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId driverNumber = do
  let isOnlyActive = Just True == mbOnlyActive
  let limit' = maybe 10 fromIntegral mbLimit
  let offset' = maybe 0 fromIntegral mbOffset
  bookings <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is BeamB.riderId $ Se.Eq personId]
              <> ([Se.Is BeamB.status $ Se.Not $ Se.In [DRB.COMPLETED, DRB.CANCELLED, DRB.REALLOCATED] | isOnlyActive])
              <> ([Se.Is BeamB.status $ Se.Eq (fromJust mbBookingStatus) | isJust mbBookingStatus])
              <> ([Se.Is BeamB.clientId $ Se.Eq (getId <$> mbClientId) | isJust mbClientId])
          )
      ]
      (Se.Desc BeamB.createdAt)
      (Just limit')
      (Just offset')
  findAllWithOptionsKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId . DRB.id <$> bookings, Se.Is BeamR.driverNumberHash $ Se.Eq (Just driverNumber)]] (Se.Desc BeamR.createdAt) (Just limit') (Just offset')

countRidesByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
countRidesByRiderId riderId = do
  booking <- findAllWithKV [Se.Is BeamB.riderId $ Se.Eq $ getId riderId]
  findAllWithKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId <$> (DRB.id <$> booking), Se.Is BeamR.status $ Se.Eq Ride.COMPLETED]] <&> length

countRidesFromDateToNowByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> UTCTime -> m Int
countRidesFromDateToNowByRiderId riderId date = do
  booking <- findAllWithKV [Se.Is BeamB.riderId $ Se.Eq $ getId riderId]
  findAllWithKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId <$> (DRB.id <$> booking), Se.Is BeamR.status $ Se.Eq Ride.COMPLETED, Se.Is BeamR.createdAt $ Se.GreaterThan date]] <&> length

updateEditLocationAttempts :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> Maybe Int -> m ()
updateEditLocationAttempts rideId attempts = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.allowedEditLocationAttempts attempts,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateEditPickupLocationAttempts :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> Maybe Int -> m ()
updateEditPickupLocationAttempts rideId attempts = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.allowedEditPickupLocationAttempts attempts,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

driverMobileNumberKey :: Text -> ByteString
driverMobileNumberKey driverMobileNumber = TE.encodeUtf8 $ "Ride:DriverMobileNumber:" <> driverMobileNumber

extractRideIds :: KVDBAnswer [ByteString] -> [Text]
extractRideIds (Right value) = TE.decodeUtf8 <$> value
extractRideIds _ = []

findLatestByDriverPhoneNumber :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Ride)
findLatestByDriverPhoneNumber driverMobileNumber = do
  let lookupKey = driverMobileNumberKey driverMobileNumber
  rideIds_ <- L.runKVDB meshConfig.kvRedis $ L.smembers lookupKey
  let rideIds = extractRideIds rideIds_
  logTagDebug "findLatestByDriverPhoneNumber" $ "RideIds for driverMobileNumber:-" <> driverMobileNumber <> ", KVDBAnswer:-" <> show rideIds_ <> ", decoded rideIds:-" <> show rideIds
  case rideIds of
    [] -> do
      logTagError "findLatestByDriverPhoneNumber" $ "Redis Lookup Empty, RideIds for driverMobileNumber:-" <> driverMobileNumber <> ", KVDBAnswer:-" <> show rideIds_ <> ", decoded rideIds:-" <> show rideIds
      findLatestActiveByDriverNumber driverMobileNumber
    _ -> findLatestActiveByRideIds rideIds
  where
    findLatestActiveByRideIds :: (MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) => [Text] -> m (Maybe Ride)
    findLatestActiveByRideIds rideIds =
      do
        findAllWithKVAndConditionalDB
          [ Se.And
              [ Se.Is BeamR.id $ Se.In rideIds,
                Se.Is BeamR.status $ Se.Eq Ride.NEW
              ]
          ]
          (Just (Se.Desc BeamR.createdAt))
        <&> listToMaybe

    findLatestActiveByDriverNumber :: (MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Ride)
    findLatestActiveByDriverNumber driverNum =
      do
        let limit = 1
        findAllWithOptionsKV
          [ Se.And
              [ Se.Is BeamR.driverMobileNumber $ Se.Eq driverNum,
                Se.Is BeamR.status $ Se.Eq Ride.NEW
              ]
          ]
          (Se.Desc BeamR.createdAt)
          (Just limit)
          Nothing
        <&> listToMaybe

appendByDriverPhoneNumber :: (MonadFlow m, Hedis.HedisFlow m r, HasField "storeRidesTimeLimit" r Int) => Ride -> m ()
appendByDriverPhoneNumber ride = do
  storeRidesTimeLimit <- asks (.storeRidesTimeLimit)
  let lookupKey = driverMobileNumberKey ride.driverMobileNumber
      rideId = [TE.encodeUtf8 $ getId ride.id]
      expTime = toInteger storeRidesTimeLimit -- 60 minutes
  void $
    L.runKVDB meshConfig.kvRedis $ do
      void $ L.sadd lookupKey rideId
      L.expire lookupKey expTime

updateshowDriversPreviousRideDropLoc :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> Id Ride -> m ()
updateshowDriversPreviousRideDropLoc showDriversPreviousRideDropLoc (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamR.showDriversPreviousRideDropLoc $ Kernel.Prelude.Just showDriversPreviousRideDropLoc, Se.Set BeamR.updatedAt _now] [Se.Is BeamR.id $ Se.Eq id]

updateSafetyJourneyStatus :: (MonadFlow m, EsqDBFlow m r) => Id Ride -> SosJourneyStatus -> m ()
updateSafetyJourneyStatus rideId status = do
  updateOneWithKV
    [ Se.Set BeamR.safetyJourneyStatus $ Just status
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

findOneByBookingId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> m (Maybe Ride)
findOneByBookingId (Id bookingId) = findAllWithOptionsKV [Se.Is BeamR.bookingId $ Se.Eq bookingId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe
