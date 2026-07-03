module SharedLogic.LocationAddressEnrichment
  ( enrichLocationAddress,
  )
where

import qualified Data.Geohash as DG
import Data.Text (pack)
import qualified Domain.Types.Extra.PlaceNameCache as DTM
import Domain.Types.LocationAddress
import Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
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
enrichLocationAddress _ _ _ address@LocationAddress {areaCode = Just _} = pure address
enrichLocationAddress merchantId merchantOperatingCityId gps address = do
  eAreaCode <- withTryCatch "enrichLocationAddress" $ resolveAreaCode merchantId merchantOperatingCityId gps address
  mbAreaCode <- case eAreaCode of
    Right areaCode -> pure areaCode
    Left err -> do
      logWarning $ "enrichLocationAddress: area-code enrichment failed, proceeding without it: " <> show err
      pure Nothing
  case mbAreaCode of
    Just areaCode -> pure address {areaCode = Just areaCode}
    Nothing -> pure address

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
