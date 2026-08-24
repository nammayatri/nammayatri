module SharedLogic.LocationAddressEnrichment
  ( enrichLocationAddress,
    mkLocationAddressFromPlaceName,
    reverseGeocodeAddress,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Geohash as DG
import qualified Data.HashMap.Strict as HM
import Data.Text (pack)
import qualified Data.Text as T
import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Extra.PlaceNameCache as DTM
import Domain.Types.LocationAddress
import Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.PlaceNameCache
import Kernel.External.Maps as Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Prometheus.Internal (incrementCounter)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Storage.CachedQueries.Maps.PlaceNameCache as CM
import qualified Storage.CachedQueries.Merchant as QM
import Tools.Error
import qualified Tools.Maps as TMaps

enrichLocationAddress ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, ServiceFlow m r, EventStreamFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  LocationAddress ->
  m LocationAddress
enrichLocationAddress _ _ _ address
  | isJust (nonEmptyAreaCode address.areaCode) = pure address
enrichLocationAddress merchantId merchantOperatingCityId gps address = do
  eAreaCode <- withTryCatch "enrichLocationAddress" $ resolveAreaCode merchantId merchantOperatingCityId gps address
  mbAreaCode <- case eAreaCode of
    Right areaCode -> pure areaCode
    Left err -> do
      logWarning $ "enrichLocationAddress: area-code enrichment failed, proceeding without it: " <> show err
      pure Nothing
  case mbAreaCode of
    Just areaCode -> pure address {areaCode = nonEmptyAreaCode (Just areaCode)}
    Nothing -> pure address {areaCode = Nothing}

-- | The provider's address for a point, or 'Nothing' if it cannot be resolved.
--
-- Goes through 'DMaps.getPlaceName' rather than the maps client directly, so a point that
-- has been named before is answered from the place-name cache instead of costing a call.
-- Never throws: a caller reverse-geocoding a location already has an address on it, and a
-- worse name beats failing whatever it was doing.
reverseGeocodeAddress ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, ServiceFlow m r, EventStreamFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  LatLong ->
  m (Maybe LocationAddress)
reverseGeocodeAddress personId merchantId latLong =
  withTryCatch "reverseGeocodeAddress" (DMaps.getPlaceName (personId, merchantId) Nothing req) >>= \case
    Right placeNames -> pure $ mkLocationAddressFromPlaceName <$> listToMaybe placeNames
    Left err -> do
      logWarning $ "reverseGeocodeAddress: could not resolve a place name at " <> show latLong <> ": " <> show err
      pure Nothing
  where
    req = TMaps.GetPlaceNameReq {getBy = TMaps.ByLatLong latLong, sessionToken = Nothing, language = Nothing}

-- | Reads a maps-provider result into the shape a location's address is stored in.
--
-- Only a fallback: the app geocodes the points it puts on the map and sends the names back
-- with them, and this fills in for the times it does not. Mirrors the component mapping
-- the driver app uses in @SharedLogic.GoogleMaps@, so the same point resolves to the same
-- address on both sides.
mkLocationAddressFromPlaceName :: Maps.PlaceName -> LocationAddress
mkLocationAddressFromPlaceName placeName =
  LocationAddress
    { street = firstOfTypes ["route", "street_address"],
      door = Nothing,
      city = firstOfTypes ["locality"],
      state = firstOfTypes ["administrative_area_level_1"],
      country = firstOfTypes ["country"],
      building = firstOfTypes ["premise", "sub_premise"],
      areaCode = nonEmptyAreaCode $ firstOfTypes ["postal_code"],
      area = firstOfTypes ["sublocality_level_5", "sublocality_level_4", "sublocality_level_3", "sublocality_level_2", "sublocality_level_1"] <|> firstOfTypes ["sublocality"],
      ward = firstOfTypes ["ward"],
      placeId = placeName.placeId,
      instructions = Nothing,
      -- The leading component of the formatted address: the building or road, rather than
      -- the locality, city and country trailing it. That is what a customer reads as the
      -- name of the place they are being picked up from.
      title = placeName.formattedAddress >>= (fmap T.strip . listToMaybe . T.splitOn ","),
      extras = Nothing
    }
  where
    -- Later components win, matching how the provider orders them narrowest-last.
    byType = HM.fromList [(componentType, component.longName) | component <- placeName.addressComponents, componentType <- component.types]
    firstOfTypes types = listToMaybe $ mapMaybe (`HM.lookup` byType) types

nonEmptyAreaCode :: Maybe Text -> Maybe Text
nonEmptyAreaCode = \case
  Nothing -> Nothing
  Just t | T.null (T.strip t) -> Nothing
  Just t -> Just t

resolveAreaCode ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, ServiceFlow m r, EventStreamFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  LocationAddress ->
  m (Maybe Text)
resolveAreaCode merchantId merchantOperatingCityId _gps LocationAddress {placeId = Just placeId} = do
  mbAreaCode <- lookupAreaCodeFromPlaceId placeId
  case mbAreaCode of
    Just areaCode -> pure $ Just areaCode
    Nothing -> fetchAreaCodeFromMapsApi merchantId merchantOperatingCityId (Maps.ByPlaceId placeId)
resolveAreaCode merchantId merchantOperatingCityId gps _ = do
  mbAreaCode <- lookupAreaCodeFromGeoHash merchantId gps
  case mbAreaCode of
    Just areaCode -> pure $ Just areaCode
    Nothing -> fetchAreaCodeFromMapsApi merchantId merchantOperatingCityId (Maps.ByLatLong gps)

lookupAreaCodeFromPlaceId ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  m (Maybe Text)
lookupAreaCodeFromPlaceId placeId = do
  (placeNameCaches, _) <- CM.findPlaceByPlaceId placeId
  pure $ listToMaybe placeNameCaches >>= extractPostalCode

lookupAreaCodeFromGeoHash ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  LatLong ->
  m (Maybe Text)
lookupAreaCodeFromGeoHash merchantId LatLong {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case DG.encode merchant.geoHashPrecisionValue (lat, lon) of
    Nothing -> pure Nothing
    Just geoHash -> do
      (placeNameCaches, _) <- CM.findPlaceByGeoHash (pack geoHash)
      pure $ listToMaybe placeNameCaches >>= extractPostalCode

fetchAreaCodeFromMapsApi ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, ServiceFlow m r, EventStreamFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maps.GetPlaceNameBy ->
  m (Maybe Text)
fetchAreaCodeFromMapsApi merchantId merchantOperatingCityId getBy = do
  deploymentVersion <- asks (.version)
  incrementCounter merchantOperatingCityId.getId "location_area_code_google_lookup" deploymentVersion.getDeploymentVersion
  placeNameResp <-
    TMaps.getPlaceName merchantId merchantOperatingCityId Nothing $
      Maps.GetPlaceNameReq
        { getBy = getBy,
          sessionToken = Nothing,
          language = Nothing
        }
  cachePlaceNameResp merchantId getBy placeNameResp
  pure $ listToMaybe placeNameResp >>= extractPostalCodeFromPlaceName

cachePlaceNameResp ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Maps.GetPlaceNameBy ->
  [Maps.PlaceName] ->
  m ()
cachePlaceNameResp merchantId getBy placeNameResp =
  whenJust (listToMaybe placeNameResp) $ \element -> do
    merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    let (latitude, longitude) = case getBy of
          Maps.ByLatLong (Maps.LatLong lat lon) -> (lat, lon)
          _ -> (element.location.lat, element.location.lon)
    placeNameCache <- buildPlaceNameCache element latitude longitude merchant.geoHashPrecisionValue
    _ <- CM.create placeNameCache
    whenJust placeNameCache.placeId $ \placeId ->
      CM.cachedPlaceByPlaceId placeId [placeNameCache]
    whenJust placeNameCache.geoHash $ \geoHash ->
      CM.cachedPlaceByGeoHash geoHash [placeNameCache]

buildPlaceNameCache :: MonadFlow m => Maps.PlaceName -> Double -> Double -> Int -> m PlaceNameCache
buildPlaceNameCache resultsResp latitude longitude geoHashPrecisionValue = do
  cacheId <- generateGUID
  now <- getCurrentTime
  pure
    PlaceNameCache
      { id = cacheId,
        formattedAddress = resultsResp.formattedAddress,
        addressComponents = map (\Maps.AddressResp {..} -> DTM.AddressResp {..}) resultsResp.addressComponents,
        plusCode = resultsResp.plusCode,
        lat = resultsResp.location.lat,
        lon = resultsResp.location.lon,
        placeId = resultsResp.placeId,
        geoHash = pack <$> DG.encode geoHashPrecisionValue (latitude, longitude),
        addressHash = Nothing,
        createdAt = now
      }

extractPostalCode :: PlaceNameCache -> Maybe Text
extractPostalCode PlaceNameCache {addressComponents} = extractPostalCodeFromComponents addressComponents

extractPostalCodeFromPlaceName :: Maps.PlaceName -> Maybe Text
extractPostalCodeFromPlaceName Maps.PlaceName {addressComponents} =
  fmap (.longName) . find (\Maps.AddressResp {types} -> "postal_code" `elem` types) $ addressComponents

extractPostalCodeFromComponents :: [DTM.AddressResp] -> Maybe Text
extractPostalCodeFromComponents =
  fmap (.longName) . find (\DTM.AddressResp {types} -> "postal_code" `elem` types)
