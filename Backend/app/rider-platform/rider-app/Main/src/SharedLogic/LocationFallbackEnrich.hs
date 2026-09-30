module SharedLogic.LocationFallbackEnrich
  ( BppLocation (..),
    BppBookingLocationsRes (..),
    FallbackSource (..),
    resolveOne,
    bookingLocations,
    patchBookingLocations,
    resolveBooking,
    mapDetails,
    detailsLocations,
    enrichBooking,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.LocationFallback
import SharedLogic.LocationFallbackTypes
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Metrics.BAPMetrics (incrementLocationFallbackServedCounter)

data FallbackSource = BookingRow | DriverApp | Placeholder
  deriving (Show, Eq)

withCoords :: DL.Location -> Double -> Double -> DLA.LocationAddress -> DL.Location
withCoords loc la lo addr = loc {DL.lat = la, DL.lon = lo, DL.address = addr}

bppAddress :: BppLocation -> DLA.LocationAddress
bppAddress b =
  DLA.LocationAddress
    { street = b.street,
      door = b.door,
      city = b.city,
      state = b.state,
      country = b.country,
      building = b.building,
      areaCode = b.areaCode,
      area = b.area,
      ward = Nothing,
      placeId = Nothing,
      instructions = b.instructions,
      title = Nothing,
      extras = b.extras
    }

fromBookingRow :: DRB.ParentSearchRequestLocationInfo -> LocationRole -> Maybe (Double, Double, DLA.LocationAddress)
fromBookingRow info = \case
  Pickup -> Just (info.sourceLat, info.sourceLon, info.sourceAddress)
  Drop -> Just (info.destLat, info.destLon, info.destAddress)
  Stop -> Nothing

fromBpp :: LocationRole -> Int -> BppBookingLocationsRes -> Maybe BppLocation
fromBpp role idx res = case role of
  Pickup -> Just res.from
  Drop -> res.to
  Stop -> listToMaybe (drop idx res.stops)

resolveOne :: Maybe DRB.ParentSearchRequestLocationInfo -> Maybe BppBookingLocationsRes -> LocationRole -> Int -> DL.Location -> Maybe (DL.Location, FallbackSource)
resolveOne mbInfo mbBpp role idx loc
  | not (isPlaceholderLocation loc) = Nothing
  | Just b <- mbBpp >>= fromBpp role idx = Just (withCoords loc b.lat b.lon (bppAddress b), DriverApp)
  | Just (la, lo, addr) <- mbInfo >>= (`fromBookingRow` role) = Just (withCoords loc la lo addr, BookingRow)
  | otherwise = Just (loc, Placeholder)

mapDetails :: (DL.Location -> DL.Location) -> ([DL.Location] -> [DL.Location]) -> DRB.BookingDetails -> DRB.BookingDetails
mapDetails f g = \case
  DRB.OneWayDetails DRB.OneWayBookingDetails {..} -> DRB.OneWayDetails DRB.OneWayBookingDetails {toLocation = f toLocation, stops = g stops, ..}
  DRB.DriverOfferDetails DRB.OneWayBookingDetails {..} -> DRB.DriverOfferDetails DRB.OneWayBookingDetails {toLocation = f toLocation, stops = g stops, ..}
  DRB.OneWaySpecialZoneDetails DRB.OneWaySpecialZoneBookingDetails {..} -> DRB.OneWaySpecialZoneDetails DRB.OneWaySpecialZoneBookingDetails {toLocation = f toLocation, stops = g stops, ..}
  DRB.InterCityDetails DRB.InterCityBookingDetails {..} -> DRB.InterCityDetails DRB.InterCityBookingDetails {toLocation = f toLocation, stops = g stops, ..}
  DRB.AmbulanceDetails DRB.AmbulanceBookingDetails {..} -> DRB.AmbulanceDetails DRB.AmbulanceBookingDetails {toLocation = f toLocation, ..}
  DRB.DeliveryDetails DRB.DeliveryBookingDetails {..} -> DRB.DeliveryDetails DRB.DeliveryBookingDetails {toLocation = f toLocation, ..}
  DRB.MeterRideDetails DRB.MeterRideBookingDetails {..} -> DRB.MeterRideDetails DRB.MeterRideBookingDetails {toLocation = f <$> toLocation, ..}
  details@(DRB.RentalDetails _) -> details
  details@(DRB.EasyBookingDetails _) -> details

detailsLocations :: DRB.BookingDetails -> ([DL.Location], [DL.Location])
detailsLocations = \case
  DRB.OneWayDetails d -> ([d.toLocation], d.stops)
  DRB.DriverOfferDetails d -> ([d.toLocation], d.stops)
  DRB.OneWaySpecialZoneDetails d -> ([d.toLocation], d.stops)
  DRB.InterCityDetails d -> ([d.toLocation], d.stops)
  DRB.AmbulanceDetails d -> ([d.toLocation], [])
  DRB.DeliveryDetails d -> ([d.toLocation], [])
  DRB.MeterRideDetails d -> (maybeToList d.toLocation, [])
  DRB.RentalDetails _ -> ([], [])
  DRB.EasyBookingDetails _ -> ([], [])

bookingLocations :: DRB.Booking -> [(LocationRole, Int, DL.Location)]
bookingLocations b =
  let (drops, stopLocs) = detailsLocations b.bookingDetails
   in [(Pickup, 0, b.fromLocation)] <> map (Drop,0,) drops <> zipWith (Stop,,) [0 ..] stopLocs

patchBookingLocations :: (LocationRole -> Int -> DL.Location -> DL.Location) -> DRB.Booking -> DRB.Booking
patchBookingLocations f b =
  b
    { DRB.fromLocation = f Pickup 0 b.fromLocation,
      DRB.initialPickupLocation = f Pickup 0 b.initialPickupLocation,
      DRB.bookingDetails = mapDetails (f Drop 0) (zipWith (f Stop) [0 ..]) b.bookingDetails
    }

resolveBooking :: Maybe BppBookingLocationsRes -> DRB.Booking -> (DRB.Booking, [(LocationRole, Id DL.Location, FallbackSource)])
resolveBooking mbBpp b =
  let pick = resolveOne b.parentSearchRequestLocationInfo mbBpp
      patched = patchBookingLocations (\r i l -> maybe l fst (pick r i l)) b
      served = mapMaybe (\(r, i, l) -> (\(_, s) -> (r, l.id, s)) <$> pick r i l) (bookingLocations b)
   in (patched, served)

data CachedBppLocations = CachedFound BppBookingLocationsRes | CachedNotFound
  deriving (Generic, Show, FromJSON, ToJSON)

cacheKey :: Text -> Text
cacheKey bppBookingId = "LocationFallback:bppBooking:" <> bppBookingId

fetchBppLocations :: DRB.Booking -> Flow (Maybe BppBookingLocationsRes)
fetchBppLocations booking = case booking.bppBookingId of
  Nothing -> pure Nothing
  Just (Id bppBookingId) ->
    Hedis.safeGet (cacheKey bppBookingId) >>= \case
      Just (CachedFound res) -> pure (Just res)
      Just CachedNotFound -> pure Nothing
      Nothing -> do
        mbMerchant <- CQM.findById booking.merchantId
        case mbMerchant of
          Nothing -> pure Nothing
          Just merchant -> do
            result <- withTryCatch "BookingLocations" (CallBPPInternal.getBookingLocations merchant.driverOfferApiKey merchant.driverOfferBaseUrl bppBookingId)
            case result of
              Left err -> do
                logError $ "LOCATION_FALLBACK_BPP_CALL_FAILED bookingId=" <> booking.id.getId <> " err=" <> show err
                Hedis.setExp (cacheKey bppBookingId) CachedNotFound 60
                pure Nothing
              Right (Just res) -> Hedis.setExp (cacheKey bppBookingId) (CachedFound res) 3600 >> pure (Just res)
              Right Nothing -> Hedis.setExp (cacheKey bppBookingId) CachedNotFound 300 >> pure Nothing

enrichBooking :: DRB.Booking -> Flow DRB.Booking
enrichBooking booking
  | not (isPlaceholderLocation booking.initialPickupLocation || any (\(_, _, l) -> isPlaceholderLocation l) (bookingLocations booking)) = pure booking
  | otherwise = do
    mbBpp <- fetchBppLocations booking
    let (patched, served) = resolveBooking mbBpp booking
    forM_ served $ \(role, locId, source) -> do
      logError $
        "LOCATION_FALLBACK_SERVED source=" <> show source
          <> " role="
          <> show role
          <> " bookingId="
          <> booking.id.getId
          <> " locationId="
          <> locId.getId
          <> " riderId="
          <> booking.riderId.getId
      incrementLocationFallbackServedCounter (show source) (show role)
    pure patched
