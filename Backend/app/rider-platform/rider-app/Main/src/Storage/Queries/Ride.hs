{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Ride where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import Domain.Types.Booking.Type (Booking)
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Ride as Ride
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Ride as BeamR
import Storage.Queries.Booking (transformBeamBookingToDomain)
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Person as Person
import Storage.Tabular.Ride as Ride

create :: L.MonadFlow m => Ride -> m (MeshResult ())
create ride = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRideToBeam ride)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- updateStatus ::
--   Id Ride ->
--   RideStatus ->
--   SqlDB ()
-- updateStatus rideId status_ = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RideUpdatedAt =. val now,
--         RideStatus =. val status_
--       ]
--     where_ $ tbl ^. RideId ==. val (getId rideId)

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Ride -> RideStatus -> m (MeshResult ())
updateStatus rideId status = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamR.status status,
          Se.Set BeamR.updatedAt now
        ]
        [Se.Is BeamR.id (Se.Eq $ getId rideId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateTrackingUrl ::
--   Id Ride ->
--   BaseUrl ->
--   SqlDB ()
-- updateTrackingUrl rideId url = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RideUpdatedAt =. val now,
--         RideTrackingUrl =. val (Just $ showBaseUrl url)
--       ]
--     where_ $ tbl ^. RideId ==. val (getId rideId)

updateTrackingUrl :: (L.MonadFlow m, MonadTime m) => Id Ride -> BaseUrl -> m (MeshResult ())
updateTrackingUrl rideId url = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamR.trackingUrl (Just $ showBaseUrl url),
          Se.Set BeamR.updatedAt now
        ]
        [Se.Is BeamR.id (Se.Eq $ getId rideId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateRideRating ::
--   Id Ride ->
--   Int ->
--   SqlDB ()
-- updateRideRating rideId rideRating = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RideUpdatedAt =. val now,
--         RideRideRating =. val (Just rideRating)
--       ]
--     where_ $ tbl ^. RideId ==. val (getId rideId)

updateRideRating :: (L.MonadFlow m, MonadTime m) => Id Ride -> Int -> m (MeshResult ())
updateRideRating rideId rideRating = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamR.rideRating (Just rideRating),
          Se.Set BeamR.updatedAt now
        ]
        [Se.Is BeamR.id (Se.Eq $ getId rideId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- findById :: Transactionable m => Id Ride -> m (Maybe Ride)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id Ride -> m (Maybe Ride)
findById (Id rideId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamR.id $ Se.Eq rideId]
      case result of
        Right ride -> traverse transformBeamRideToDomain ride
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findByBPPRideId :: Transactionable m => Id BPPRide -> m (Maybe Ride)
-- findByBPPRideId bppRideId_ =
--   findOne $ do
--     ride <- from $ table @RideT
--     where_ $ ride ^. RideBppRideId ==. val (getId bppRideId_)
--     return ride

findByBPPRideId :: L.MonadFlow m => Id BPPRide -> m (Maybe Ride)
findByBPPRideId bppRideId_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamR.bppRideId $ Se.Eq $ getId bppRideId_]
      case result of
        Right ride -> traverse transformBeamRideToDomain ride
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- updateMultiple :: Id Ride -> Ride -> SqlDB ()
-- updateMultiple rideId ride = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RideUpdatedAt =. val now,
--         RideStatus =. val ride.status,
--         RideFare =. val (realToFrac <$> ride.fare),
--         RideTotalFare =. val (realToFrac <$> ride.totalFare),
--         RideChargeableDistance =. val ride.chargeableDistance,
--         RideRideStartTime =. val ride.rideStartTime,
--         RideRideEndTime =. val ride.rideEndTime
--       ]
--     where_ $ tbl ^. RideId ==. val (getId rideId)

updateMultiple :: (L.MonadFlow m, MonadTime m) => Id Ride -> Ride -> m (MeshResult ())
updateMultiple rideId ride = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamR.status ride.status,
          Se.Set BeamR.fare (realToFrac <$> ride.fare),
          Se.Set BeamR.totalFare (realToFrac <$> ride.totalFare),
          Se.Set BeamR.chargeableDistance ride.chargeableDistance,
          Se.Set BeamR.rideStartTime ride.rideStartTime,
          Se.Set BeamR.rideEndTime ride.rideEndTime,
          Se.Set BeamR.updatedAt now
        ]
        [Se.Is BeamR.id (Se.Eq $ getId rideId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- findActiveByRBId :: Transactionable m => Id Booking -> m (Maybe Ride)
-- findActiveByRBId rbId =
--   findOne $ do
--     ride <- from $ table @RideT
--     where_ $
--       ride ^. RideBookingId ==. val (toKey rbId)
--         &&. ride ^. RideStatus !=. val CANCELLED
--     return ride

findActiveByRBId :: L.MonadFlow m => Id Booking -> m (Maybe Ride)
findActiveByRBId (Id rbId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamR.bookingId $ Se.Eq rbId, Se.Is BeamR.status $ Se.Eq Ride.CANCELLED]]
      case result of
        Right ride -> traverse transformBeamRideToDomain ride
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findAllByRBId :: Transactionable m => Id Booking -> m [Ride]
-- findAllByRBId bookingId =
--   findAll $ do
--     ride <- from $ table @RideT
--     where_ $ ride ^. RideBookingId ==. val (toKey bookingId)
--     orderBy [desc $ ride ^. RideCreatedAt]
--     return ride

findAllByRBId :: L.MonadFlow m => Id Booking -> m [Ride]
findAllByRBId (Id bookingId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig [Se.Is BeamR.bookingId $ Se.Eq bookingId] (Se.Desc BeamR.createdAt) Nothing Nothing
      case result of
        Right rides -> traverse transformBeamRideToDomain rides
        Left _ -> pure []
    Nothing -> pure []

-- updateDriverArrival :: Id Ride -> SqlDB ()
-- updateDriverArrival rideId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RideDriverArrivalTime =. val (Just now),
--         RideUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. RideTId ==. val (toKey rideId)

updateDriverArrival :: (L.MonadFlow m, MonadTime m) => Id Ride -> m (MeshResult ())
updateDriverArrival rideId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamR.driverArrivalTime (Just now),
          Se.Set BeamR.updatedAt now
        ]
        [Se.Is BeamR.id (Se.Eq $ getId rideId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

upcoming6HrsCond :: SqlExpr (Entity RideT) -> UTCTime -> SqlExpr (Esq.Value Bool)
upcoming6HrsCond ride now = ride ^. RideCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    riderId :: Id Person
  }

-- findStuckRideItems :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [StuckRideItem]
-- findStuckRideItems merchantId bookingIds now = do
--   res <- Esq.findAll $ do
--     ride :& booking <-
--       from $
--         table @RideT
--           `innerJoin` table @BookingT
--             `Esq.on` ( \(ride :& booking) ->
--                          ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
--                      )
--     where_ $
--       booking ^. BookingMerchantId ==. val (toKey merchantId)
--         &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
--         &&. (ride ^. RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now)
--     pure (ride ^. RideTId, booking ^. BookingTId, booking ^. BookingRiderId)
--   pure $ mkStuckRideItem <$> res
--   where
--     mkStuckRideItem (rideId, bookingId, riderId) = StuckRideItem {..}

findStuckRideItems :: (L.MonadFlow m, MonadTime m, Log m) => Id Merchant -> [Id Booking] -> UTCTime -> m [StuckRideItem]
findStuckRideItems (Id merchantId) bookingIds now = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  let now6HrBefore = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
  case dbConf of
    Just dbCOnf' -> do
      rides <- do
        res <-
          KV.findAllWithKVConnector
            dbCOnf'
            updatedMeshConfig
            [ Se.And
                [ Se.Is BeamR.status $ Se.Eq Ride.NEW,
                  Se.Is BeamR.createdAt $ Se.LessThanOrEq now6HrBefore
                ]
            ]
        case res of
          Left _ -> pure []
          Right x -> traverse transformBeamRideToDomain x
      bookings <- do
        let modelNameB = Se.modelTableName @BeamB.BookingT
        let updatedMeshConfigB = setMeshConfig modelNameB
        res <-
          KV.findAllWithKVConnector
            dbCOnf'
            updatedMeshConfigB
            [ Se.And
                [ Se.Is BeamB.providerId $ Se.Eq merchantId,
                  Se.Is BeamB.id $ Se.In $ getId <$> bookingIds
                ]
            ]
        case res of
          Left _ -> pure []
          Right x -> catMaybes <$> traverse transformBeamBookingToDomain x
      let rideBooking = foldl' (getRideWithBooking bookings) [] rides

      pure $ mkStuckRideItem <$> rideBooking
    Nothing -> pure []
  where
    getRideWithBooking bookings acc ride' =
      let bookings' = filter (\x -> x.id == ride'.bookingId) bookings
       in acc <> ((\x -> (ride'.id, x.id, x.riderId)) <$> bookings')

    mkStuckRideItem (rideId, bookingId, riderId) = StuckRideItem {..}

-- cancelRides :: [Id Ride] -> UTCTime -> SqlDB ()
-- cancelRides rideIds now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RideStatus =. val CANCELLED,
--         RideUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. RideTId `in_` valList (toKey <$> rideIds)

cancelRides :: (L.MonadFlow m, MonadTime m) => [Id Ride] -> UTCTime -> m ()
cancelRides rideIds now = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamR.RideT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamR.status Ride.CANCELLED,
            Se.Set BeamR.updatedAt now
          ]
          [Se.Is BeamR.id (Se.In $ getId <$> rideIds)]
    Nothing -> pure ()

data RideItem = RideItem
  { person :: Person,
    ride :: Ride,
    bookingStatus :: Common.BookingStatus
  }

findAllRideItems ::
  Transactionable m =>
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
findAllRideItems merchantId limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhone mbFrom mbTo now = do
  res <- Esq.findAll $ do
    booking :& ride :& person <-
      from $
        table @BookingT
          `innerJoin` table @RideT
            `Esq.on` ( \(booking :& ride) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
          `innerJoin` table @PersonT
            `Esq.on` ( \(booking :& _ :& person) ->
                         booking ^. Booking.BookingRiderId ==. person ^. Person.PersonTId
                     )
    let bookingStatusVal = mkBookingStatusVal ride
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
        &&. whenJust_ mbBookingStatus (\bookingStatus -> bookingStatusVal ==. val bookingStatus)
        &&. whenJust_ mbRideShortId (\rideShortId -> ride ^. Ride.RideShortId ==. val rideShortId.getShortId)
        &&. whenJust_ mbCustomerPhoneDBHash (\hash -> person ^. Person.PersonMobileNumberHash ==. val (Just hash))
        &&. whenJust_ mbDriverPhone (\driverMobileNumber -> ride ^. Ride.RideDriverMobileNumber ==. val driverMobileNumber)
        &&. whenJust_ mbFrom (\defaultFrom -> ride ^. Ride.RideCreatedAt >=. val defaultFrom)
        &&. whenJust_ mbTo (\defaultTo -> ride ^. Ride.RideCreatedAt <=. val defaultTo)
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return
      ( person,
        ride,
        bookingStatusVal
      )
  pure $ mkRideItem <$> res
  where
    mkBookingStatusVal ride = do
      -- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.rideStartTime = Nothing
      let ongoing6HrsCond =
            ride ^. Ride.RideRideStartTime +. just (Esq.interval [Esq.HOUR 6]) <=. val (Just now)
      case_
        [ when_ (ride ^. Ride.RideStatus ==. val Ride.NEW &&. not_ (upcoming6HrsCond ride now)) then_ $ val Common.UPCOMING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now) then_ $ val Common.UPCOMING_6HRS,
          when_ (ride ^. Ride.RideStatus ==. val Ride.INPROGRESS &&. not_ ongoing6HrsCond) then_ $ val Common.ONGOING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.COMPLETED) then_ $ val Common.RCOMPLETED,
          when_ (ride ^. Ride.RideStatus ==. val Ride.CANCELLED) then_ $ val Common.RCANCELLED
        ]
        (else_ $ val Common.ONGOING_6HRS)
    mkRideItem (person, ride, bookingStatus) = do
      RideItem {..}

-- countRides :: Transactionable m => Id Merchant -> m Int
-- countRides merchantId =
--   mkCount <$> do
--     Esq.findAll $ do
--       (_ride :& booking) <-
--         from $
--           table @RideT
--             `innerJoin` table @BookingT
--               `Esq.on` ( \(ride :& booking) ->
--                            ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
--                        )
--       where_ $ booking ^. BookingMerchantId ==. val (toKey merchantId)
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

findRiderIdByRideId :: Transactionable m => Id Ride -> m (Maybe (Id Person))
findRiderIdByRideId rideId = findOne $ do
  ride :& booking <-
    from $
      table @RideT
        `innerJoin` table @BookingT
          `Esq.on` ( \(ride :& booking) ->
                       ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                   )
  where_ $
    ride ^. RideTId ==. val (toKey rideId)
  pure $ booking ^. BookingRiderId

transformBeamRideToDomain :: L.MonadFlow m => BeamR.Ride -> m Ride
transformBeamRideToDomain BeamR.RideT {..} = do
  tUrl <- parseBaseUrl `mapM` trackingUrl
  pure $
    Ride
      { id = Id id,
        bppRideId = Id bppRideId,
        bookingId = Id bookingId,
        shortId = ShortId shortId,
        merchantId = Id <$> merchantId,
        status = status,
        driverName = driverName,
        driverRating = driverRating,
        driverMobileNumber = driverMobileNumber,
        driverRegisteredAt = driverRegisteredAt,
        vehicleNumber = vehicleNumber,
        vehicleModel = vehicleModel,
        vehicleColor = vehicleColor,
        vehicleVariant = vehicleVariant,
        otp = otp,
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
        driverMobileCountryCode = driverMobileCountryCode
      }

transformDomainRideToBeam :: Ride -> BeamR.Ride
transformDomainRideToBeam Ride {..} =
  BeamR.defaultRide
    { BeamR.id = getId id,
      BeamR.bppRideId = getId bppRideId,
      BeamR.bookingId = getId bookingId,
      BeamR.shortId = getShortId shortId,
      BeamR.merchantId = getId <$> merchantId,
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
      BeamR.driverMobileCountryCode = driverMobileCountryCode
    }
