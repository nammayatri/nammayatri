module Storage.Queries.Transformers.Booking where

import qualified Data.Text as T
import Domain.Types
import qualified Domain.Types.Booking
import qualified Domain.Types.BookingLocation as DBBL
import qualified Domain.Types.DeliveryPersonDetails as DPD
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.RiderDetails as DRDD
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.BookingLocation as QBBL
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.RiderDetails as QRDD
import Tools.Error

getTripCategory :: (Domain.Types.Booking.BookingType -> Kernel.Prelude.Maybe TripCategory -> TripCategory)
getTripCategory _bookingType _tripCategory = case _tripCategory of
  Just cat -> cat
  Nothing -> do
    case _bookingType of
      Domain.Types.Booking.NormalBooking -> OneWay OneWayOnDemandDynamicOffer
      Domain.Types.Booking.SpecialZoneBooking -> OneWay OneWayRideOtp

fromAndToLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [DLM.LocationMapping] -> Maybe TripCategory -> Text -> Maybe Text -> Maybe Text -> Text -> Maybe Text -> m (DL.Location, Maybe DL.Location)
fromAndToLocation mappings tripCategory id fromLocationId toLocationId providerId merchantOperatingCityId = do
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
      fromLocationMapping <- QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
      fl <- QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)

      tl <- do
        mbToLocationMapping <- QLM.getLatestEndByEntityId id
        maybe (pure Nothing) (QL.findById . (.locationId)) mbToLocationMapping

      return (fl, tl)

getBookingTypeFromTripCategory :: TripCategory -> Domain.Types.Booking.BookingType
getBookingTypeFromTripCategory tripCategory =
  case tripCategory of
    OneWay OneWayRideOtp -> Domain.Types.Booking.SpecialZoneBooking
    Rental RideOtp -> Domain.Types.Booking.SpecialZoneBooking
    InterCity OneWayRideOtp _ -> Domain.Types.Booking.SpecialZoneBooking
    _ -> Domain.Types.Booking.NormalBooking

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

getSenderAndReceiverDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe TripCategory -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m (Maybe (DPD.DeliveryPersonDetails, DPD.DeliveryPersonDetails))
getSenderAndReceiverDetails tripCategory senderId senderName receiverId receiverName = do
  if (isJust tripCategory && (isDeliveryTrip $ fromJust tripCategory))
    then do
      senderRiderDetail <- QRDD.findById `mapM` (Id <$> senderId) >>= fromMaybeM (InternalError $ "Sender not found in Rider Details " <> show senderId)
      receiverRiderDetail <- QRDD.findById `mapM` (Id <$> receiverId) >>= fromMaybeM (InternalError $ "Receiver not found in Rider Details " <> show receiverId)
      senderName' <- fromMaybeM (InternalError "Sender Name not found") senderName
      receiverName' <- fromMaybeM (InternalError "Receiver Name not found") receiverName
      sdr <- fromMaybeM (InternalError "Sender not found") senderRiderDetail
      rcd <- fromMaybeM (InternalError "Receiver not found") receiverRiderDetail
      return $ Just (mkDeliveryPersonDetails sdr senderName', mkDeliveryPersonDetails rcd receiverName')
    else return Nothing
  where
    mkDeliveryPersonDetails :: DRDD.RiderDetails -> Text -> DPD.DeliveryPersonDetails
    mkDeliveryPersonDetails riderDetail name =
      DPD.DeliveryPersonDetails
        { DPD.id = riderDetail.id,
          DPD.name = name,
          DPD.phone = riderDetail.mobileNumber
        }
