module Storage.Queries.Transformers.Booking where

import Control.Applicative
import Data.List (sortBy)
import Data.Ord
import Domain.Types.Booking
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingLocation as DBBL
import Domain.Types.Common
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.BookingLocation as QBBL
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

getDistance :: Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Types.Common.Distance
getDistance = \case
  DRB.OneWayDetails details -> Just details.distance
  DRB.RentalDetails _ -> Nothing
  DRB.DeliveryDetails details -> Just details.distance
  DRB.DriverOfferDetails details -> Just details.distance
  DRB.OneWaySpecialZoneDetails details -> Just details.distance
  DRB.InterCityDetails details -> Just details.distance
  DRB.AmbulanceDetails details -> Just details.distance
  DRB.OneWayScheduledDetails details -> Just details.distance

-- TODO :: Deprecated, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
getFareProductType :: Domain.Types.Booking.BookingDetails -> FareProductType
getFareProductType = \case
  DRB.OneWayDetails _ -> ONE_WAY
  DRB.RentalDetails _ -> RENTAL
  DRB.OneWaySpecialZoneDetails _ -> ONE_WAY_SPECIAL_ZONE
  DRB.InterCityDetails _ -> INTER_CITY
  DRB.AmbulanceDetails _ -> AMBULANCE
  _ -> DRIVER_OFFER

getOtpCode :: Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getOtpCode = \case
  DRB.OneWayDetails _ -> Nothing
  DRB.RentalDetails details -> details.otpCode
  DRB.DriverOfferDetails _ -> Nothing
  DRB.OneWaySpecialZoneDetails details -> details.otpCode
  DRB.InterCityDetails details -> details.otpCode
  DRB.AmbulanceDetails _ -> Nothing
  DRB.DeliveryDetails details -> details.otpCode
  DRB.OneWayScheduledDetails details -> details.otpCode

getStopLocationId :: Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getStopLocationId = \case
  DRB.OneWayDetails _ -> Nothing
  DRB.RentalDetails rentalDetails -> getId . (.id) <$> rentalDetails.stopLocation
  DRB.DriverOfferDetails _ -> Nothing
  DRB.OneWaySpecialZoneDetails _ -> Nothing
  DRB.InterCityDetails _ -> Nothing
  DRB.AmbulanceDetails _ -> Nothing
  DRB.DeliveryDetails _ -> Nothing
  DRB.OneWayScheduledDetails _ -> Nothing

getToLocationId :: Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getToLocationId = \case
  DRB.OneWayDetails details -> Just (getId details.toLocation.id)
  DRB.RentalDetails _ -> Nothing
  DRB.DriverOfferDetails details -> Just (getId details.toLocation.id)
  DRB.OneWaySpecialZoneDetails details -> Just (getId details.toLocation.id)
  DRB.InterCityDetails details -> Just (getId details.toLocation.id)
  DRB.AmbulanceDetails details -> Just (getId details.toLocation.id)
  DRB.DeliveryDetails details -> Just (getId details.toLocation.id)
  DRB.OneWayScheduledDetails details -> Just (getId details.toLocation.id)

getDeliveryBookingInfo :: Domain.Types.Booking.BookingDetails -> Maybe Domain.Types.Booking.DeliveryBookingDetails
getDeliveryBookingInfo = \case
  DRB.DeliveryDetails details -> Just details
  _ -> Nothing

backfillMOCId :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Text -> m (Id DMOC.MerchantOperatingCity)
backfillMOCId merchantOperatingCityId merchantId = case merchantOperatingCityId of
  Just mocId -> pure $ Id mocId
  Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

getInitialPickupLocation :: (CacheFlow m r, EsqDBFlow m r) => [DLM.LocationMapping] -> DL.Location -> m DL.Location
getInitialPickupLocation mappings fl = do
  let pickupLocationMap = filter (\map1 -> map1.order == 0) mappings
      sortedPickupLocationMap = sortBy (comparing (.version)) pickupLocationMap
  if null sortedPickupLocationMap
    then pure fl
    else do
      let initialPickupLocMapping = last sortedPickupLocationMap
      QL.findById initialPickupLocMapping.locationId >>= fromMaybeM (InternalError "Incorrect Location Mapping")

toBookingDetailsAndFromLocation ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  Text ->
  Maybe Text ->
  [DLM.LocationMapping] ->
  Maybe HighPrecMeters ->
  FareProductType ->
  Maybe TripCategory ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe DistanceUnit ->
  Maybe HighPrecDistance ->
  m (DL.Location, BookingDetails)
toBookingDetailsAndFromLocation id merchantId merchantOperatingCityId mappings distance fareProductType mbTripCategory toLocationId fromLocationId stopLocationId otpCode distanceUnit distanceValue = do
  logTagDebug ("bookingId:-" <> id) $ "Location Mappings:-" <> show mappings
  if null mappings
    then do
      -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
      logInfo "Accessing Booking Location Table"
      pickupLoc <- upsertFromLocationAndMappingForOldData (Id <$> fromLocationId) id merchantId merchantOperatingCityId
      upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
      bookingDetails <- case mbTripCategory of
        Just tripCategory ->
          case tripCategory of
            OneWay OneWayRideOtp -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocationId
            RideShare RideOtp -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocationId
            Rental _ -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
            InterCity _ _ -> DRB.InterCityDetails <$> buildInterCityDetails toLocationId
            Ambulance _ -> DRB.AmbulanceDetails <$> buildAmbulanceDetails toLocationId
            Delivery _ -> DRB.DeliveryDetails <$> buildDeliveryDetails toLocationId
            _ -> DRB.DriverOfferDetails <$> buildOneWayDetails toLocationId
        Nothing ->
          case fareProductType of
            ONE_WAY -> DRB.OneWayDetails <$> buildOneWayDetails toLocationId
            RENTAL -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
            ONE_WAY_SPECIAL_ZONE -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocationId
            INTER_CITY -> DRB.InterCityDetails <$> buildInterCityDetails toLocationId
            AMBULANCE -> DRB.AmbulanceDetails <$> buildAmbulanceDetails toLocationId
            _ -> DRB.DriverOfferDetails <$> buildOneWayDetails toLocationId
      return (pickupLoc, bookingDetails)
    else do
      fromLocationMapping <- QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
      fl <- QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)

      mbToLocationMapping <- QLM.getLatestEndByEntityId id
      let toLocId = (.locationId.getId) <$> mbToLocationMapping

      logTagDebug ("bookingId:-" <> id) $ "To Location Mapping:-" <> show mbToLocationMapping
      logTagDebug ("bookingId:-" <> id) $ "To Location Id:-" <> show toLocId

      bookingDetails <- case mbTripCategory of
        Just tripCategory ->
          case tripCategory of
            OneWay OneWayRideOtp -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocId
            RideShare RideOtp -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocId
            Rental _ -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
            InterCity _ _ -> DRB.InterCityDetails <$> buildInterCityDetails toLocId
            Ambulance _ -> DRB.AmbulanceDetails <$> buildAmbulanceDetails toLocId
            Delivery _ -> DRB.DeliveryDetails <$> buildDeliveryDetails toLocationId
            _ -> DRB.DriverOfferDetails <$> buildOneWayDetails toLocId
        Nothing ->
          case fareProductType of
            ONE_WAY -> DRB.OneWayDetails <$> buildOneWayDetails toLocId
            RENTAL -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
            DRIVER_OFFER -> DRB.DriverOfferDetails <$> buildOneWayDetails toLocId
            ONE_WAY_SPECIAL_ZONE -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocId
            INTER_CITY -> DRB.InterCityDetails <$> buildInterCityDetails toLocId
            AMBULANCE -> DRB.AmbulanceDetails <$> buildAmbulanceDetails toLocId
      return (fl, bookingDetails)
  where
    buildOneWayDetails mbToLocid = do
      toLocid <- mbToLocid & fromMaybeM (InternalError $ "toLocationId is null for one way bookingId:-" <> id)
      toLocation <- maybe (pure Nothing) (QL.findById . Id) (Just toLocid) >>= fromMaybeM (InternalError "toLocation is null for one way booking")
      distance' <- (mkDistanceWithDefault distanceUnit distanceValue <$> distance) & fromMaybeM (InternalError "distance is null for one way booking")
      pure
        DRB.OneWayBookingDetails
          { toLocation = toLocation,
            distance = distance'
          }
    buildInterCityDetails mbToLocid = do
      toLocid <- mbToLocid & fromMaybeM (InternalError $ "toLocationId is null for one way bookingId:-" <> id)
      toLocation <- maybe (pure Nothing) (QL.findById . Id) (Just toLocid) >>= fromMaybeM (InternalError "toLocation is null for one way booking")
      distance' <- (mkDistanceWithDefault distanceUnit distanceValue <$> distance) & fromMaybeM (InternalError "distance is null for one way booking")
      pure
        DRB.InterCityBookingDetails
          { toLocation = toLocation,
            distance = distance',
            ..
          }
    buildOneWaySpecialZoneDetails mbToLocid = do
      toLocid <- mbToLocid & fromMaybeM (InternalError $ "toLocationId is null for one way bookingId:-" <> id)
      toLocation <- maybe (pure Nothing) (QL.findById . Id) (Just toLocid) >>= fromMaybeM (InternalError "toLocation is null for one way special zone booking")
      distance' <- (mkDistanceWithDefault distanceUnit distanceValue <$> distance) & fromMaybeM (InternalError "distance is null for one way booking")
      pure
        DRB.OneWaySpecialZoneBookingDetails
          { distance = distance',
            toLocation = toLocation,
            ..
          }
    buildRentalDetails mbStopLocationId = do
      mbStopLocation <- maybe (pure Nothing) (QL.findById . Id) mbStopLocationId
      pure
        DRB.RentalBookingDetails
          { stopLocation = mbStopLocation,
            ..
          }
    buildAmbulanceDetails mbToLocid = do
      toLocid <- mbToLocid & fromMaybeM (InternalError $ "toLocationId is null for one way ambulance bookingId:-" <> id)
      toLocation <- maybe (pure Nothing) (QL.findById . Id) (Just toLocid) >>= fromMaybeM (InternalError "toLocation is null for one way ambulance booking")
      distance' <- (mkDistanceWithDefault distanceUnit distanceValue <$> distance) & fromMaybeM (InternalError "distance is null for one way ambulance booking")
      pure
        DRB.AmbulanceBookingDetails
          { toLocation = toLocation,
            distance = distance'
          }
    buildDeliveryDetails mbToLocid = do
      toLocid <- mbToLocid & fromMaybeM (InternalError $ "toLocationId is null for delivery bookingId:-" <> id)
      toLocation <- maybe (pure Nothing) (QL.findById . Id) (Just toLocid) >>= fromMaybeM (InternalError "toLocation is null for delivery booking")
      distance' <- (mkDistanceWithDefault distanceUnit distanceValue <$> distance) & fromMaybeM (InternalError "distance is null for delivery booking")
      pure
        DRB.DeliveryBookingDetails
          { toLocation = toLocation,
            distance = distance',
            ..
          }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME

buildLocation :: (MonadFlow m, EsqDBFlow m r) => DBBL.BookingLocation -> m DL.Location
buildLocation DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        ..
      }

upsertFromLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe (Id DBBL.BookingLocation) -> Text -> Text -> Maybe Text -> m DL.Location
upsertFromLocationAndMappingForOldData locationId bookingId merchantId merchantOperatingCityId = do
  loc <- QBBL.findById `mapM` locationId >>= fromMaybeM (InternalError "From Location Id Not Found in Booking Table")
  pickupLoc <- maybe (throwError $ InternalError ("From Location Not Found in Booking Location Table for BookingId : " <> bookingId)) buildLocation loc
  fromLocationMapping <- SLM.buildPickUpLocationMapping pickupLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create pickupLoc >> QLM.upsert fromLocationMapping
  return pickupLoc

upsertToLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Text -> Text -> Maybe Text -> m ()
upsertToLocationAndMappingForOldData toLocationId bookingId merchantId merchantOperatingCityId = do
  toLocation <- maybe (pure Nothing) (QBBL.findById . Id) toLocationId >>= fromMaybeM (InternalError "toLocation is null for one way booking")
  dropLoc <- buildLocation toLocation
  toLocationMapping <- SLM.buildDropLocationMapping dropLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create dropLoc >> QLM.create toLocationMapping
