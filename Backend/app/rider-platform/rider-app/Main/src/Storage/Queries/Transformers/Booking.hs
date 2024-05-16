{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Transformers.Booking where

import Control.Applicative
import Data.List (sortBy)
import Data.Ord
import Domain.Types.Booking
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingLocation as DBBL
import Domain.Types.FarePolicy.FareProductType as DFF
import qualified Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
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

getDistance :: (Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Types.Common.Distance)
getDistance = \case
  DRB.OneWayDetails details -> Just details.distance
  DRB.RentalDetails _ -> Nothing
  DRB.DriverOfferDetails details -> Just details.distance
  DRB.OneWaySpecialZoneDetails details -> Just details.distance
  DRB.InterCityDetails details -> Just details.distance

getFareProductType :: (Domain.Types.Booking.BookingDetails -> Domain.Types.FarePolicy.FareProductType.FareProductType)
getFareProductType = \case
  DRB.OneWayDetails _ -> DQuote.ONE_WAY
  DRB.RentalDetails _ -> DQuote.RENTAL
  DRB.DriverOfferDetails _ -> DQuote.DRIVER_OFFER
  DRB.OneWaySpecialZoneDetails _ -> DQuote.ONE_WAY_SPECIAL_ZONE
  DRB.InterCityDetails _ -> DQuote.INTER_CITY

getOtpCode :: (Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Prelude.Text)
getOtpCode = \case
  DRB.OneWayDetails _ -> Nothing
  DRB.RentalDetails _ -> Nothing
  DRB.DriverOfferDetails _ -> Nothing
  DRB.OneWaySpecialZoneDetails details -> details.otpCode
  DRB.InterCityDetails _ -> Nothing

getStopLocationId :: (Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Prelude.Text)
getStopLocationId = \case
  DRB.OneWayDetails _ -> Nothing
  DRB.RentalDetails rentalDetails -> (getId . (.id)) <$> rentalDetails.stopLocation
  DRB.DriverOfferDetails _ -> Nothing
  DRB.OneWaySpecialZoneDetails _ -> Nothing
  DRB.InterCityDetails _ -> Nothing

getToLocationId :: (Domain.Types.Booking.BookingDetails -> Kernel.Prelude.Maybe Kernel.Prelude.Text)
getToLocationId = \case
  DRB.OneWayDetails details -> Just (getId details.toLocation.id)
  DRB.RentalDetails _ -> Nothing
  DRB.DriverOfferDetails details -> Just (getId details.toLocation.id)
  DRB.OneWaySpecialZoneDetails details -> Just (getId details.toLocation.id)
  DRB.InterCityDetails details -> Just (getId details.toLocation.id)

backfillMOCId merchantOperatingCityId merchantId = case merchantOperatingCityId of
  Just mocId -> pure $ Id mocId
  Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

getInitialPickupLocation mappings fl = do
  let pickupLocationMap = filter (\map1 -> map1.order == 0) mappings
      sortedPickupLocationMap = sortBy (comparing (.version)) pickupLocationMap
  if null sortedPickupLocationMap
    then pure fl
    else do
      let initialPickupLocMapping = last sortedPickupLocationMap
      QL.findById initialPickupLocMapping.locationId >>= fromMaybeM (InternalError "Incorrect Location Mapping")

fromLocationAndBookingDetails id merchantId merchantOperatingCityId mappings distance fareProductType toLocationId fromLocationId stopLocationId otpCode distanceUnit distanceValue = do
  logTagDebug ("bookingId:-" <> id) $ "Location Mappings:-" <> show mappings
  if null mappings
    then do
      -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
      logInfo "Accessing Booking Location Table"
      pickupLoc <- upsertFromLocationAndMappingForOldData (Id <$> fromLocationId) id merchantId merchantOperatingCityId
      bookingDetails <- case fareProductType of
        DFF.ONE_WAY -> do
          upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
          DRB.OneWayDetails <$> buildOneWayDetails toLocationId
        DFF.RENTAL -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
        DFF.DRIVER_OFFER -> do
          upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
          DRB.OneWayDetails <$> buildOneWayDetails toLocationId
        DFF.ONE_WAY_SPECIAL_ZONE -> do
          upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
          DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocationId
        DFF.INTER_CITY -> do
          upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
          DRB.InterCityDetails <$> buildInterCityDetails toLocationId
      return (pickupLoc, bookingDetails)
    else do
      fromLocationMapping <- QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
      fl <- QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)

      mbToLocationMapping <- QLM.getLatestEndByEntityId id
      let toLocId = (.locationId.getId) <$> mbToLocationMapping

      logTagDebug ("bookingId:-" <> id) $ "To Location Mapping:-" <> show mbToLocationMapping
      logTagDebug ("bookingId:-" <> id) $ "To Location Id:-" <> show toLocId

      bookingDetails <- case fareProductType of
        DFF.ONE_WAY -> DRB.OneWayDetails <$> buildOneWayDetails toLocId
        DFF.RENTAL -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
        DFF.DRIVER_OFFER -> DRB.OneWayDetails <$> buildOneWayDetails toLocId
        DFF.ONE_WAY_SPECIAL_ZONE -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocId
        DFF.INTER_CITY -> DRB.InterCityDetails <$> buildInterCityDetails toLocId
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
            distance = distance'
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
          { stopLocation = mbStopLocation
          }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME

buildLocation :: KvDbFlow m r => DBBL.BookingLocation -> m DL.Location
buildLocation DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        ..
      }

upsertFromLocationAndMappingForOldData :: KvDbFlow m r => Maybe (Id DBBL.BookingLocation) -> Text -> Text -> Maybe Text -> m DL.Location
upsertFromLocationAndMappingForOldData locationId bookingId merchantId merchantOperatingCityId = do
  loc <- QBBL.findById `mapM` locationId >>= fromMaybeM (InternalError "From Location Id Not Found in Booking Table")
  pickupLoc <- maybe (throwError $ InternalError ("From Location Not Found in Booking Location Table for BookingId : " <> bookingId)) buildLocation loc
  fromLocationMapping <- SLM.buildPickUpLocationMapping pickupLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create pickupLoc >> QLM.create fromLocationMapping
  return pickupLoc

upsertToLocationAndMappingForOldData :: KvDbFlow m r => Maybe Text -> Text -> Text -> Maybe Text -> m ()
upsertToLocationAndMappingForOldData toLocationId bookingId merchantId merchantOperatingCityId = do
  toLocation <- maybe (pure Nothing) (QBBL.findById . Id) toLocationId >>= fromMaybeM (InternalError "toLocation is null for one way booking")
  dropLoc <- buildLocation toLocation
  toLocationMapping <- SLM.buildDropLocationMapping dropLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create dropLoc >> QLM.create toLocationMapping
