{-# LANGUAGE InstanceSigs #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Booking where

import Data.Ord
import qualified Data.Text as T
import Domain.Types.Booking
import qualified Domain.Types.Booking.BookingLocation as DBBL
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QueriesFP
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM

createBooking' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
createBooking' = createWithKV

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
create dBooking = do
  void $ whenNothingM_ (QL.findById dBooking.fromLocation.id) $ do QL.create dBooking.fromLocation
  whenJust dBooking.toLocation $ \toLocation -> whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
  createBooking' dBooking

createBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
createBooking booking = do
  fromLocationMap <- SLM.buildPickUpLocationMapping booking.fromLocation.id booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId)
  QLM.create fromLocationMap
  whenJust booking.toLocation $ \toLocation -> do
    toLocationMaps <- SLM.buildDropLocationMapping toLocation.id booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId)
    QLM.create toLocationMaps
  create booking

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

findBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m (Maybe Booking)
findBySTId searchTryId = do
  mbDriverQuote <- QDQuote.findDriverQuoteBySTId searchTryId
  maybe (pure Nothing) (\dQ -> findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq $ getId $ DDQ.id dQ]) mbDriverQuote

findByQuoteId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe Booking)
findByQuoteId quoteId = findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq quoteId]

updateStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.status rbStatus, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateStop :: MonadFlow m => Id Booking -> Maybe Text -> m ()
updateStop bookingId stopLocationId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.stopLocationId stopLocationId,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateStopArrival :: MonadFlow m => Id Booking -> m ()
updateStopArrival bookingId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.stopLocationId Nothing,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateRiderId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Id RiderDetails -> m ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.riderId $ Just $ getId riderId, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateRiderName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Text -> m ()
updateRiderName bookingId riderName = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamB.riderName $ Just riderName, Se.Set BeamB.updatedAt now] [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateSpecialZoneOtpCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Text -> m ()
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.specialZoneOtpCode $ Just specialZoneOtpCode, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

findStuckBookings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Merchant ->
  DMOC.MerchantOperatingCity ->
  [Id Booking] ->
  UTCTime ->
  m [Id Booking]
findStuckBookings merchant moCity bookingIds now = do
  let updatedTimestamp = addUTCTime (- (6 * 60 * 60)) now
  (Domain.Types.Booking.id <$>)
    <$> findAllWithDb
      [ Se.And
          [ Se.Is BeamB.providerId (Se.Eq merchant.id.getId),
            Se.Is BeamB.id (Se.In (getId <$> bookingIds)),
            Se.Is BeamB.status (Se.In [NEW, TRIP_ASSIGNED]),
            Se.Is BeamB.createdAt (Se.LessThanOrEq updatedTimestamp),
            Se.Or
              ( [Se.Is BeamB.merchantOperatingCityId $ Se.Eq $ Just (getId moCity.id)]
                  <> [Se.Is BeamB.merchantOperatingCityId $ Se.Eq Nothing | moCity.city == merchant.city]
              )
          ]
      ]

findBookingBySpecialZoneOTP :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Text -> UTCTime -> Int -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now specialZoneBookingOtpExpiry = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now specialZoneBookingOtpExpiry
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Text -> UTCTime -> Int -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP (Id merchantId) otpCode now bookingOtpExpiry = do
  let otpExpiryCondition = addUTCTime (- (fromIntegral bookingOtpExpiry * 60) :: NominalDiffTime) now
  (Domain.Types.Booking.id <$>) <$> findOneWithKV [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.providerId $ Se.Eq merchantId, Se.Is BeamB.createdAt $ Se.GreaterThanOrEq otpExpiryCondition, Se.Is BeamB.status $ Se.Eq NEW]]

cancelBookings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [Se.Set BeamB.status CANCELLED, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

findFareForCancelledBookings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Booking] -> m Money
findFareForCancelledBookings bookingIds = findAllWithKV [Se.And [Se.Is BeamB.status $ Se.Eq CANCELLED, Se.Is BeamB.id $ Se.In $ getId <$> bookingIds]] <&> sum . map Domain.Types.Booking.estimatedFare

findLastCancelledByRiderId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RiderDetails -> m (Maybe Booking)
findLastCancelledByRiderId riderDetailsId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamB.riderId (Se.Eq (Just riderDetailsId.getId)),
          Se.Is BeamB.status (Se.Eq CANCELLED)
        ]
    ]
    (Se.Desc BeamB.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

instance FromTType' BeamB.Booking Booking where
  fromTType' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BeamB.Booking -> m (Maybe Booking)
  fromTType' BeamB.BookingT {..} = do
    mappings <- QLM.findByEntityId id
    let tripCategory' = case tripCategory of
          Just cat -> cat
          Nothing -> do
            case bookingType of
              NormalBooking -> DTC.OneWay DTC.OneWayOnDemandDynamicOffer
              SpecialZoneBooking -> DTC.OneWay DTC.OneWayRideOtp

    (fl, tl) <-
      case (mappings, tripCategory) of
        ([], Nothing) -> do
          -- HANDLING OLD DATA : ONLY IF TripCategory is Nothing as for older cases
          logInfo "Accessing Booking Location Table"
          pickupLoc <- upsertLocationForOldData (Id <$> fromLocationId) id
          pickupLocMapping <- SLM.buildPickUpLocationMapping pickupLoc.id id DLM.BOOKING (Just $ Id providerId) (Id <$> merchantOperatingCityId)
          QLM.create pickupLocMapping

          dropLoc <- upsertLocationForOldData (Id <$> toLocationId) id
          dropLocMapping <- SLM.buildDropLocationMapping dropLoc.id id DLM.BOOKING (Just $ Id providerId) (Id <$> merchantOperatingCityId)
          QLM.create dropLocMapping
          return (pickupLoc, Just dropLoc)
        _ -> do
          let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
          fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
          fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> fromLocMap.locationId.getId)

          tl <- do
            let toLocationMappings = filter (\loc -> loc.order /= 0) mappings
            if (null toLocationMappings)
              then return Nothing
              else do
                let toLocMap = maximumBy (comparing (.order)) toLocationMappings
                QL.findById toLocMap.locationId
          return (fl, tl)

    fp <- QueriesFP.findById (Id fareParametersId)
    pUrl <- parseBaseUrl bapUri
    merchant <- CQM.findById (Id providerId) >>= fromMaybeM (MerchantNotFound providerId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant bapCity
    if isJust fp
      then
        pure $
          Just
            Booking
              { id = Id id,
                transactionId = transactionId,
                quoteId = quoteId,
                status = status,
                tripCategory = tripCategory',
                specialLocationTag = specialLocationTag,
                specialZoneOtpCode = specialZoneOtpCode,
                disabilityTag = disabilityTag,
                area = area,
                providerId = Id providerId,
                merchantOperatingCityId = merchantOpCityId,
                searchRequestId = Id <$> searchRequestId,
                primaryExophone = primaryExophone,
                bapId = bapId,
                bapUri = pUrl,
                bapCity = bapCity,
                bapCountry = bapCountry,
                startTime = startTime,
                riderId = Id <$> riderId,
                fromLocation = fl,
                toLocation = tl,
                vehicleVariant = vehicleVariant,
                estimatedDistance = estimatedDistance,
                maxEstimatedDistance = maxEstimatedDistance,
                estimatedFare = estimatedFare,
                estimatedDuration = estimatedDuration,
                fareParams = fromJust fp, -- This fromJust is safe because of the check above.
                paymentMethodId = Id <$> paymentMethodId,
                riderName = riderName,
                paymentUrl = paymentUrl,
                createdAt = createdAt,
                updatedAt = updatedAt,
                stopLocationId = Id <$> stopLocationId,
                isScheduled = fromMaybe False isScheduled,
                distanceToPickup = distanceToPickup
              }
      else do
        logError $ "FareParameters not found for booking: " <> show id
        pure Nothing

instance ToTType' BeamB.Booking Booking where
  toTType' Booking {..} = do
    -- This booking type is just for backward compatibility (We don't have it in Domain Type)
    let bookingType = case tripCategory of
          DTC.OneWay DTC.OneWayRideOtp -> SpecialZoneBooking
          _ -> NormalBooking

    BeamB.BookingT
      { BeamB.id = getId id,
        BeamB.transactionId = transactionId,
        BeamB.quoteId = quoteId,
        BeamB.status = status,
        BeamB.bookingType = bookingType,
        BeamB.tripCategory = Just tripCategory,
        BeamB.specialLocationTag = specialLocationTag,
        BeamB.disabilityTag = disabilityTag,
        BeamB.specialZoneOtpCode = specialZoneOtpCode,
        BeamB.area = area,
        BeamB.providerId = getId providerId,
        BeamB.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        BeamB.searchRequestId = getId <$> searchRequestId,
        BeamB.primaryExophone = primaryExophone,
        BeamB.bapId = bapId,
        BeamB.bapUri = showBaseUrl bapUri,
        BeamB.startTime = startTime,
        BeamB.riderId = getId <$> riderId,
        BeamB.bapCity = bapCity,
        BeamB.bapCountry = bapCountry,
        BeamB.fromLocationId = Just $ getId fromLocation.id,
        BeamB.toLocationId = (getId . (.id)) <$> toLocation,
        BeamB.vehicleVariant = vehicleVariant,
        BeamB.estimatedDistance = estimatedDistance,
        BeamB.maxEstimatedDistance = maxEstimatedDistance,
        BeamB.estimatedFare = estimatedFare,
        BeamB.estimatedDuration = estimatedDuration,
        BeamB.fareParametersId = getId fareParams.id,
        BeamB.paymentMethodId = getId <$> paymentMethodId,
        BeamB.paymentUrl = paymentUrl,
        BeamB.riderName = riderName,
        BeamB.createdAt = createdAt,
        BeamB.updatedAt = updatedAt,
        BeamB.distanceToPickup = distanceToPickup,
        BeamB.isScheduled = Just isScheduled,
        BeamB.stopLocationId = getId <$> stopLocationId
      }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
buildLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DBBL.BookingLocation -> m DL.Location
buildLocation DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        address = mkLocationAddress address,
        ..
      }

mkLocationAddress :: DBBL.LocationAddress -> DL.LocationAddress
mkLocationAddress DBBL.LocationAddress {..} =
  DL.LocationAddress
    { fullAddress = mkFullAddress DBBL.LocationAddress {..},
      ..
    }

mkFullAddress :: DBBL.LocationAddress -> Maybe Text
mkFullAddress DBBL.LocationAddress {..} = do
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, city, state, areaCode, country]
  if null strictFields
    then Nothing
    else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.replace " " "")

upsertLocationForOldData :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe (Id DBBL.BookingLocation) -> Text -> m DL.Location
upsertLocationForOldData locationId bookingId = do
  loc <- QBBL.findById `mapM` locationId >>= fromMaybeM (InternalError "Location Id Not Found in Booking Location Table")
  location <- maybe (throwError $ InternalError ("Location Not Found in Booking Location Table for BookingId : " <> bookingId)) buildLocation loc
  void $ QL.create location
  return location
