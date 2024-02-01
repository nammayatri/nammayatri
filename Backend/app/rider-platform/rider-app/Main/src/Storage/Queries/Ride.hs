{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Ride where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import Data.Ord
import qualified Database.Beam as B
import Database.Beam.Backend (autoSqlValueSyntax)
import qualified Database.Beam.Backend as BeamBackend
import Domain.Types.Booking.Type as Booking
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride as Ride
import qualified EulerHS.Language as L
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Ride as BeamR
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.Person ()

createRide' :: MonadFlow m => Ride -> m ()
createRide' = createWithKV

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Ride -> m ()
create ride = do
  _ <- whenNothingM_ (QL.findById ride.fromLocation.id) $ do QL.create ride.fromLocation
  _ <- whenJust ride.toLocation $ \location -> processLocation location
  createRide' ride
  where
    processLocation location = whenNothingM_ (QL.findById location.id) $ do QL.create location

createRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Ride -> m ()
createRide ride = do
  fromLocationMap <- SLM.buildPickUpLocationMapping ride.fromLocation.id ride.id.getId DLM.RIDE ride.merchantId ride.merchantOperatingCityId
  mbToLocationMap <- maybe (pure Nothing) (\detail -> Just <$> SLM.buildDropLocationMapping detail.id ride.id.getId DLM.RIDE ride.merchantId ride.merchantOperatingCityId) ride.toLocation
  void $ QLM.create fromLocationMap
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap
  create ride

data DatabaseWith3 table1 table2 table3 f = DatabaseWith3
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2),
    dwTable3 :: f (B.TableEntity table3)
  }
  deriving (Generic, B.Database be)

updateStatus :: MonadFlow m => Id Ride -> RideStatus -> m ()
updateStatus rideId status = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.status status,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateTrackingUrl :: MonadFlow m => Id Ride -> BaseUrl -> m ()
updateTrackingUrl rideId url = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.trackingUrl (Just $ showBaseUrl url),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateRideRating :: MonadFlow m => Id Ride -> Int -> m ()
updateRideRating rideId rideRating = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.rideRating (Just rideRating),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ride -> m (Maybe Ride)
findById (Id rideId) = findOneWithKV [Se.Is BeamR.id $ Se.Eq rideId]

findByBPPRideId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BPPRide -> m (Maybe Ride)
findByBPPRideId bppRideId_ = findOneWithKV [Se.Is BeamR.bppRideId $ Se.Eq $ getId bppRideId_]

updateMultiple :: MonadFlow m => Id Ride -> Ride -> m ()
updateMultiple rideId ride = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.status ride.status,
      Se.Set BeamR.fare (realToFrac <$> ride.fare),
      Se.Set BeamR.totalFare (realToFrac <$> ride.totalFare),
      Se.Set BeamR.chargeableDistance ride.chargeableDistance,
      Se.Set BeamR.rideStartTime ride.rideStartTime,
      Se.Set BeamR.rideEndTime ride.rideEndTime,
      Se.Set BeamR.endOtp ride.endOtp,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

findActiveByRBId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m (Maybe Ride)
findActiveByRBId (Id rbId) = findOneWithKV [Se.And [Se.Is BeamR.bookingId $ Se.Eq rbId, Se.Is BeamR.status $ Se.Not $ Se.Eq Ride.CANCELLED]]

findByRBId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m (Maybe Ride)
findByRBId (Id bookingId) = findOneWithKV [Se.Is BeamR.bookingId $ Se.Eq bookingId]

findLatestCompletedRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Ride)
findLatestCompletedRide riderId = do
  booking <- findAllWithOptionsKV [Se.Is BeamB.riderId $ Se.Eq $ getId riderId] (Se.Desc BeamB.createdAt) Nothing Nothing
  findAllWithOptionsKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId <$> (DRB.id <$> booking), Se.Is BeamR.status $ Se.Eq Ride.COMPLETED]] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

updateDriverArrival :: MonadFlow m => Id Ride -> m ()
updateDriverArrival rideId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.driverArrivalTime (Just now),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateSafetyCheckStatus :: MonadFlow m => Id Ride -> Maybe Bool -> m ()
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
            Se.Is BeamB.fareProductType $ Se.Not $ Se.Eq DQuote.RENTAL,
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

cancelRides :: MonadFlow m => [Id Ride] -> UTCTime -> m ()
cancelRides rideIds now =
  updateWithKV
    [ Se.Set BeamR.status Ride.CANCELLED,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.In $ getId <$> rideIds)]

data RideItem = RideItem
  { person :: Person,
    ride :: Ride,
    bookingStatus :: Common.BookingStatus
  }

instance BeamBackend.BeamSqlBackend be => B.HasSqlEqualityCheck be Common.BookingStatus

instance BeamBackend.HasSqlValueSyntax be String => BeamBackend.HasSqlValueSyntax be Common.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

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
  dbConf <- getMasterBeamConfig
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
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultFrom -> B.sqlBool_ $ ride.createdAt B.>=. B.val_ defaultFrom) mbFrom
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultTo -> B.sqlBool_ $ ride.createdAt B.<=. B.val_ defaultTo) mbTo
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\bookingStatus -> mkBookingStatusVal ride B.==?. B.val_ bookingStatus) mbBookingStatus
              )
              do
                booking' <- B.all_ (BeamCommon.booking BeamCommon.atlasDB)
                ride' <- B.join_' (BeamCommon.ride BeamCommon.atlasDB) (\ride'' -> BeamR.bookingId ride'' B.==?. BeamB.id booking')
                person' <- B.join_' (BeamCommon.person BeamCommon.atlasDB) (\person'' -> BeamP.id person'' B.==?. BeamB.riderId booking')
                pure (booking', ride', person')
  res' <- case res of
    Right x -> do
      let rides = snd' <$> x
          persons = thd' <$> x
      r <- catMaybes <$> mapM fromTType' rides
      p <- catMaybes <$> mapM fromTType' persons
      pure $ zip3 p r (mkBookingStatus now <$> r)
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
    snd' (_, y, _) = y
    thd' (_, _, z) = z
    mkRideItem (person, ride, bookingStatus) =
      RideItem {..}

findRiderIdByRideId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ride -> m (Maybe (Id Person))
findRiderIdByRideId rideId = do
  ride <- findOneWithKV [Se.Is BeamR.id $ Se.Eq $ getId rideId]
  booking <- maybe (pure Nothing) (\ride' -> findOneWithKV [Se.Is BeamB.id $ Se.Eq $ getId (Ride.bookingId ride')]) ride
  pure $ Booking.riderId <$> booking

findAllByRiderIdAndRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> m [Booking]
findAllByRiderIdAndRide (Id personId) mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  let isOnlyActive = Just True == mbOnlyActive
  let limit' = maybe 10 fromIntegral mbLimit
  let offset' = maybe 0 fromIntegral mbOffset
  bookings <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is BeamB.riderId $ Se.Eq personId]
              <> ([Se.Is BeamB.status $ Se.Not $ Se.In [DRB.COMPLETED, DRB.CANCELLED] | isOnlyActive])
              <> ([Se.Is BeamB.status $ Se.Eq (fromJust mbBookingStatus) | isJust mbBookingStatus])
          )
      ]
      (Se.Desc BeamB.createdAt)
      (Just limit')
      (Just offset')

  rides <- findAllWithOptionsKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId . DRB.id <$> bookings]] (Se.Desc BeamR.createdAt) Nothing Nothing
  let filteredBookings = matchBookingsWithRides bookings rides
  let filteredB = filterBookingsWithConditions filteredBookings
  pure $ take limit' filteredB
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
           in isJust maybeRide || isJust otpCode && isNothing maybeRide

countRidesByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
countRidesByRiderId riderId = do
  booking <- findAllWithKV [Se.Is BeamB.riderId $ Se.Eq $ getId riderId]
  findAllWithKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId <$> (DRB.id <$> booking), Se.Is BeamR.status $ Se.Eq Ride.COMPLETED]] <&> length

countRidesFromDateToNowByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> UTCTime -> m Int
countRidesFromDateToNowByRiderId riderId date = do
  booking <- findAllWithKV [Se.Is BeamB.riderId $ Se.Eq $ getId riderId]
  findAllWithKV [Se.And [Se.Is BeamR.bookingId $ Se.In $ getId <$> (DRB.id <$> booking), Se.Is BeamR.status $ Se.Eq Ride.COMPLETED, Se.Is BeamR.createdAt $ Se.GreaterThan date]] <&> length

findRideByRideShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => ShortId Ride -> m (Maybe Ride)
findRideByRideShortId (ShortId shortId) = findOneWithKV [Se.Is BeamR.shortId $ Se.Eq shortId]

updateEditLocationAttempts :: MonadFlow m => Id Ride -> Maybe Int -> m ()
updateEditLocationAttempts rideId attempts = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.allowedEditLocationAttempts attempts,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

createMapping :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> Maybe Text -> Maybe Text -> m [DLM.LocationMapping]
createMapping bookingId rideId merchantId merchantOperatingCityId = do
  mappings <- QLM.findByEntityId bookingId
  when (null mappings) $ throwError (InternalError "Entity Mappings for Booking Not Found") -- this case should never occur
  let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
      toLocationMappings = filter (\loc -> loc.order /= 0) mappings

  fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")

  fromLocationRideMapping <- SLM.buildPickUpLocationMapping fromLocMap.locationId rideId DLM.RIDE (Id <$> merchantId) (Id <$> merchantOperatingCityId)
  QLM.create fromLocationRideMapping

  toLocationRideMappings <-
    if not (null toLocationMappings)
      then do
        let toLocMap = maximumBy (comparing (.order)) toLocationMappings
        toLocationRideMapping <- SLM.buildDropLocationMapping toLocMap.locationId rideId DLM.RIDE (Id <$> merchantId) (Id <$> merchantOperatingCityId)
        QLM.create toLocationRideMapping
        return [toLocationRideMapping]
      else return []

  return $ fromLocationRideMapping : toLocationRideMappings

instance FromTType' BeamR.Ride Ride where
  fromTType' BeamR.RideT {..} = do
    mappings <- QLM.findByEntityId id
    rideMappings <-
      if null mappings
        then do
          void $ QBooking.findById (Id bookingId)
          createMapping bookingId id merchantId merchantOperatingCityId
        else return mappings

    let fromLocationMapping = filter (\loc -> loc.order == 0) rideMappings
        toLocationMappings = filter (\loc -> loc.order /= 0) rideMappings

    fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")

    fromLocation <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in ride for fromLocationId: " <> fromLocMap.locationId.getId)
    toLocation <-
      if null toLocationMappings
        then return Nothing
        else do
          let toLocMap = maximumBy (comparing (.order)) toLocationMappings
          QL.findById toLocMap.locationId
    tUrl <- parseBaseUrl `mapM` trackingUrl
    pure $
      Just
        Ride
          { id = Id id,
            bppRideId = Id bppRideId,
            bookingId = Id bookingId,
            shortId = ShortId shortId,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            status = status,
            driverName = driverName,
            driverRating = driverRating,
            driverMobileNumber = driverMobileNumber,
            driverRegisteredAt = driverRegisteredAt,
            driverImage = driverImage,
            vehicleNumber = vehicleNumber,
            vehicleModel = vehicleModel,
            vehicleColor = vehicleColor,
            vehicleVariant = vehicleVariant,
            otp = otp,
            endOtp = endOtp,
            trackingUrl = tUrl,
            fare = roundToIntegral <$> fare,
            totalFare = roundToIntegral <$> totalFare,
            chargeableDistance = chargeableDistance,
            traveledDistance = traveledDistance,
            driverArrivalTime = driverArrivalTime,
            rideStartTime = rideStartTime,
            rideEndTime = rideEndTime,
            rideRating = rideRating,
            createdAt = createdAt,
            updatedAt = updatedAt,
            driverMobileCountryCode = driverMobileCountryCode,
            allowedEditLocationAttempts = allowedEditLocationAttempts,
            ..
          }

instance ToTType' BeamR.Ride Ride where
  toTType' Ride {..} =
    BeamR.RideT
      { BeamR.id = getId id,
        BeamR.bppRideId = getId bppRideId,
        BeamR.bookingId = getId bookingId,
        BeamR.shortId = getShortId shortId,
        BeamR.merchantId = getId <$> merchantId,
        BeamR.merchantOperatingCityId = getId <$> merchantOperatingCityId,
        BeamR.status = status,
        BeamR.driverName = driverName,
        BeamR.driverRating = driverRating,
        BeamR.driverMobileNumber = driverMobileNumber,
        BeamR.driverRegisteredAt = driverRegisteredAt,
        BeamR.vehicleNumber = vehicleNumber,
        BeamR.vehicleModel = vehicleModel,
        BeamR.vehicleColor = vehicleColor,
        BeamR.vehicleVariant = vehicleVariant,
        BeamR.otp = otp,
        BeamR.endOtp = endOtp,
        BeamR.trackingUrl = showBaseUrl <$> trackingUrl,
        BeamR.fare = realToFrac <$> fare,
        BeamR.totalFare = realToFrac <$> totalFare,
        BeamR.chargeableDistance = chargeableDistance,
        BeamR.traveledDistance = traveledDistance,
        BeamR.driverArrivalTime = driverArrivalTime,
        BeamR.rideStartTime = rideStartTime,
        BeamR.rideEndTime = rideEndTime,
        BeamR.rideRating = rideRating,
        BeamR.createdAt = createdAt,
        BeamR.updatedAt = updatedAt,
        BeamR.driverMobileCountryCode = driverMobileCountryCode,
        BeamR.driverImage = driverImage,
        BeamR.safetyCheckStatus = safetyCheckStatus,
        BeamR.allowedEditLocationAttempts = allowedEditLocationAttempts,
        BeamR.isFreeRide = isFreeRide
      }
