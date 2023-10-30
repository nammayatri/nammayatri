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
import Domain.Types.Booking
import qualified Domain.Types.Booking.BookingLocation as DBBL
import Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
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
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QueriesFP
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM

createBooking' :: MonadFlow m => Booking -> m ()
createBooking' = createWithKV

create :: MonadFlow m => Booking -> m ()
create dBooking = do
  case dBooking.bookingDetails of
    DetailsOnDemand BookingDetailsOnDemand {..} -> do
      _ <- whenNothingM_ (QL.findById dBooking.fromLocation.id) $ do QL.create dBooking.fromLocation
      whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
    DetailsRental BookingDetailsRental {..} -> do
      whenNothingM_ (QL.findById dBooking.fromLocation.id) $ do QL.create dBooking.fromLocation
      forM_ rentalToLocation \rentalToLocation' -> do
        whenNothingM_ (QL.findById rentalToLocation'.id) $ do QL.create rentalToLocation'
  createBooking' dBooking

createBooking :: MonadFlow m => Booking -> m ()
createBooking booking = do
  case booking.bookingDetails of
    DetailsOnDemand BookingDetailsOnDemand {..} -> do
      fromLocationMap <- SLM.buildPickUpLocationMapping booking.fromLocation.id booking.id.getId DLM.BOOKING
      toLocationMaps <- SLM.buildDropLocationMapping toLocation.id booking.id.getId DLM.BOOKING
      QLM.create fromLocationMap >> QLM.create toLocationMaps >> create booking
    DetailsRental BookingDetailsRental {..} -> do
      fromLocationMap <- SLM.buildPickUpLocationMapping booking.fromLocation.id booking.id.getId DLM.BOOKING
      toLocationMap <- forM rentalToLocation \rentalToLocation' -> do
        SLM.buildDropLocationMapping rentalToLocation'.id booking.id.getId DLM.BOOKING
      QLM.create fromLocationMap >> whenJust toLocationMap QLM.create >> create booking

findById :: MonadFlow m => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

findBySTId :: MonadFlow m => Id DST.SearchTry -> m (Maybe Booking)
findBySTId searchTryId = do
  mbDriverQuote <- QDQuote.findDriverQuoteBySTId searchTryId
  maybe (pure Nothing) (\dQ -> findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq $ getId $ DDQ.id dQ]) mbDriverQuote

updateStatus :: MonadFlow m => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.status rbStatus, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateRiderId :: MonadFlow m => Id Booking -> Id RiderDetails -> m ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.riderId $ Just $ getId riderId, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateRiderName :: MonadFlow m => Id Booking -> Text -> m ()
updateRiderName bookingId riderName = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamB.riderName $ Just riderName, Se.Set BeamB.updatedAt now] [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateSpecialZoneOtpCode :: MonadFlow m => Id Booking -> Text -> m ()
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.specialZoneOtpCode $ Just specialZoneOtpCode, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

findStuckBookings :: MonadFlow m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings (Id merchantId) bookingIds now = do
  let updatedTimestamp = addUTCTime (- (6 * 60 * 60)) now
  (Domain.Types.Booking.id <$>)
    <$> findAllWithDb
      [ Se.And
          [ Se.Is BeamB.providerId (Se.Eq merchantId),
            Se.Is BeamB.id (Se.In (getId <$> bookingIds)),
            Se.Is BeamB.status (Se.In [NEW, TRIP_ASSIGNED]),
            Se.Is BeamB.bookingType $ Se.Not $ Se.Eq RentalBooking,
            Se.Is BeamB.createdAt (Se.LessThanOrEq updatedTimestamp)
          ]
      ]

findBookingBySpecialZoneOTP :: MonadFlow m => Id Merchant -> Text -> UTCTime -> Int -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now specialZoneBookingOtpExpiry = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now specialZoneBookingOtpExpiry
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: MonadFlow m => Id Merchant -> Text -> UTCTime -> Int -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP (Id merchantId) otpCode now bookingOtpExpiry = do
  let otpExpiryCondition = addUTCTime (- (fromIntegral bookingOtpExpiry * 60) :: NominalDiffTime) now
  (Domain.Types.Booking.id <$>) <$> findOneWithKV [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.providerId $ Se.Eq merchantId, Se.Is BeamB.createdAt $ Se.GreaterThanOrEq otpExpiryCondition, Se.Is BeamB.status $ Se.Eq NEW]]

cancelBookings :: MonadFlow m => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [Se.Set BeamB.status CANCELLED, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

findFareForCancelledBookings :: MonadFlow m => [Id Booking] -> m Money
findFareForCancelledBookings bookingIds = findAllWithKV [Se.And [Se.Is BeamB.status $ Se.Eq CANCELLED, Se.Is BeamB.id $ Se.In $ getId <$> bookingIds]] <&> sum . map Domain.Types.Booking.estimatedFare

instance FromTType' BeamB.Booking Booking where
  fromTType' :: MonadFlow m => BeamB.Booking -> m (Maybe Booking)
  fromTType' BeamB.BookingT {..} = do
    fp <- QueriesFP.findById (Id fareParametersId)
    pUrl <- parseBaseUrl bapUri
    mappings <- QLM.findByEntityId id
    (fl, bookingDetails) <- case bookingType of
      RentalBooking -> do
        let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
        let toLocationMappings = filter (\loc -> loc.order /= 0) mappings

        fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
        fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> fromLocMap.locationId.getId)
        rentalToLocation <-
          if null toLocationMappings
            then pure Nothing
            else do
              let toLocMap = maximumBy (comparing (.order)) toLocationMappings
              Just <$> (QL.findById toLocMap.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in booking for toLocationId: " <> toLocMap.locationId.getId))

        let bookingDetails = DetailsRental BookingDetailsRental {rentalToLocation}
        return (fl, bookingDetails)
      _ -> do
        unless (isJust toLocationId) $ throwError (InternalError "OnDemand should have to location")
        (fl, tl) <-
          if null mappings -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
            then do
              logInfo "Accessing Booking Location Table"
              pickupLoc <- upsertLocationForOldData (Id <$> fromLocationId) id
              pickupLocMapping <- SLM.buildPickUpLocationMapping pickupLoc.id id DLM.BOOKING
              QLM.create pickupLocMapping

              dropLoc <- upsertLocationForOldData (Id <$> toLocationId) id
              dropLocMapping <- SLM.buildDropLocationMapping dropLoc.id id DLM.BOOKING
              QLM.create dropLocMapping
              return (pickupLoc, dropLoc)
            else do
              let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
                  toLocationMappings = filter (\loc -> loc.order /= 0) mappings

              fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
              fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> fromLocMap.locationId.getId)

              when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
              let toLocMap = maximumBy (comparing (.order)) toLocationMappings
              tl <- QL.findById toLocMap.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in booking for toLocationId: " <> toLocMap.locationId.getId)
              return (fl, tl)
        let bookingDetails =
              DetailsOnDemand
                BookingDetailsOnDemand
                  { specialLocationTag = specialLocationTag,
                    specialZoneOtpCode = specialZoneOtpCode,
                    toLocation = tl
                  }
        return (fl, bookingDetails)
    case fp of
      Just fareParams ->
        pure $
          Just
            Booking
              { id = Id id,
                providerId = Id providerId,
                bapUri = pUrl,
                riderId = Id <$> riderId,
                fromLocation = fl,
                paymentMethodId = Id <$> paymentMethodId,
                ..
              }
      Nothing -> do
        logError $ "FareParameters not found for booking: " <> show id
        pure Nothing

instance ToTType' BeamB.Booking Booking where
  toTType' Booking {..} = do
    let (specialLocationTag, specialZoneOtpCode, toLocationId) = case bookingDetails of
          DetailsOnDemand details -> do
            let specialLocationTag' = details.specialLocationTag
                specialZoneOtpCode' = details.specialZoneOtpCode
                toLocationId' = Just $ details.toLocation.id.getId
            (specialLocationTag', specialZoneOtpCode', toLocationId')
          DetailsRental details -> do
            let specialLocationTag' = Nothing
                specialZoneOtpCode' = Nothing
                toLocationId' = details.rentalToLocation <&> (.id.getId)
            (specialLocationTag', specialZoneOtpCode', toLocationId')
    BeamB.BookingT
      { BeamB.id = id.getId,
        BeamB.providerId = providerId.getId,
        BeamB.bapUri = showBaseUrl bapUri,
        BeamB.riderId = getId <$> riderId,
        BeamB.fromLocationId = Just fromLocation.id.getId,
        BeamB.fareParametersId = fareParams.id.getId,
        BeamB.paymentMethodId = getId <$> paymentMethodId,
        ..
      }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
buildLocation :: MonadFlow m => DBBL.BookingLocation -> m DL.Location
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
    { fullAddress = Nothing,
      ..
    }

upsertLocationForOldData :: MonadFlow m => Maybe (Id DBBL.BookingLocation) -> Text -> m DL.Location
upsertLocationForOldData locationId bookingId = do
  loc <- QBBL.findById `mapM` locationId >>= fromMaybeM (InternalError "Location Id Not Found in Booking Location Table")
  location <- maybe (throwError $ InternalError ("Location Not Found in Booking Location Table for BookingId : " <> bookingId)) buildLocation loc
  void $ QL.create location
  return location
