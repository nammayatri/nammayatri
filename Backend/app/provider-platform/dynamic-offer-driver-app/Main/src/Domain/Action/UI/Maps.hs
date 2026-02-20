{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Maps
  ( Maps.GetPlaceNameReq,
    Maps.GetPlaceNameResp,
    getPlaceName,
    AutoCompleteReq,
    Maps.AutoCompleteResp,
    autoComplete,
  )
where

import Control.Lens ((^?), _head)
import qualified Data.Geohash as DG
import Data.Text (pack)
import qualified Data.Time as DT
import Domain.Action.UI.PlaceNameCache as DTM
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.PlaceNameCache as DTM
import qualified Kernel.External.Maps.Interface.Types as MIT
import Kernel.External.Maps.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Maps.PlaceNameCache as CM
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMOC
import qualified Tools.Maps as Maps

data AutoCompleteReq = AutoCompleteReq
  { input :: Text,
    sessionToken :: Maybe Text,
    location :: Text,
    radius :: Integer,
    types_ :: Maybe Text,
    radiusWithUnit :: Maybe Distance,
    language :: Maps.Language,
    strictbounds :: Maybe Bool,
    origin :: Maybe Maps.LatLong
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

expirePlaceNameCache :: ServiceFlow m r => [PlaceNameCache] -> Id DMOC.MerchantOperatingCity -> m ()
expirePlaceNameCache placeNameCache merchantOpCityId = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
  whenJust transporterConfig.placeNameCacheExpiryDays $ \cacheExpiry -> do
    currentTime <- liftIO DT.getCurrentTime
    let expiryDate = DT.addUTCTime (DT.nominalDay * fromIntegral (- cacheExpiry)) currentTime
    let toBeDeletedPlaceNameCache = filter (\obj -> obj.createdAt < expiryDate) placeNameCache
    mapM_ CM.delete toBeDeletedPlaceNameCache

getPlaceName :: ServiceFlow m r => Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maps.GetPlaceNameReq -> m Maps.GetPlaceNameResp
getPlaceName merchantId merchantOpCityId entityId req = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case req.getBy of
    MIT.ByLatLong (Maps.LatLong lat lon) -> do
      let myGeohash = DG.encode merchant.geoHashPrecisionValue (lat, lon)
      case myGeohash of
        Just geoHash -> do
          placeNameCache <- CM.findPlaceByGeoHash (pack geoHash)
          fork "Place Name Cache Expiry" $ expirePlaceNameCache placeNameCache merchantOpCityId
          if null placeNameCache
            then callMapsApi merchantId merchantOpCityId req merchant.geoHashPrecisionValue entityId
            else pure $ map convertToGetPlaceNameResp placeNameCache
        Nothing -> callMapsApi merchantId merchantOpCityId req merchant.geoHashPrecisionValue entityId
    MIT.ByPlaceId placeId -> do
      placeNameCache <- CM.findPlaceByPlaceId placeId
      fork "Place Name Cache Expiry" $ expirePlaceNameCache placeNameCache merchantOpCityId
      if null placeNameCache
        then callMapsApi merchantId merchantOpCityId req merchant.geoHashPrecisionValue entityId
        else pure $ map convertToGetPlaceNameResp placeNameCache

callMapsApi :: ServiceFlow m r => Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Maps.GetPlaceNameReq -> Int -> Maybe Text -> m Maps.GetPlaceNameResp
callMapsApi merchantId merchantOpCityId req geoHashPrecisionValue entityId = do
  res <- Maps.getPlaceName merchantId merchantOpCityId entityId req
  let firstElement = res ^? _head
  whenJust firstElement $ \element -> do
    let (latitude, longitude) = case req.getBy of
          MIT.ByLatLong (Maps.LatLong lat lon) -> (lat, lon)
          _ -> (element.location.lat, element.location.lon)
    placeNameCache <- convertResultsRespToPlaceNameCache element latitude longitude geoHashPrecisionValue
    _ <- CM.create placeNameCache
    whenJust placeNameCache.placeId $ \placeid -> do
      CM.cachedPlaceByPlaceId placeid [placeNameCache]
    whenJust placeNameCache.geoHash $ \geohash -> do
      CM.cachedPlaceByGeoHash geohash [placeNameCache]
  return res

convertToGetPlaceNameResp :: PlaceNameCache -> Maps.PlaceName
convertToGetPlaceNameResp placeNameCache =
  MIT.PlaceName
    { formattedAddress = placeNameCache.formattedAddress,
      addressComponents = map (\DTM.AddressResp {..} -> MIT.AddressResp {..}) placeNameCache.addressComponents,
      plusCode = placeNameCache.plusCode,
      location = LatLong {lat = placeNameCache.lat, lon = placeNameCache.lon},
      placeId = placeNameCache.placeId,
      source = Nothing
    }

convertResultsRespToPlaceNameCache :: MonadFlow m => MIT.PlaceName -> Double -> Double -> Int -> m DTM.PlaceNameCache
convertResultsRespToPlaceNameCache resultsResp latitude longitude geoHashPrecisionValue = do
  id <- generateGUID
  now <- getCurrentTime
  let res =
        DTM.PlaceNameCache
          { id,
            formattedAddress = resultsResp.formattedAddress,
            addressComponents = map (\MIT.AddressResp {..} -> DTM.AddressResp {..}) resultsResp.addressComponents,
            plusCode = resultsResp.plusCode,
            lat = resultsResp.location.lat,
            lon = resultsResp.location.lon,
            placeId = resultsResp.placeId,
            geoHash = pack <$> DG.encode geoHashPrecisionValue (latitude, longitude),
            createdAt = now
          }
  return res

autoComplete :: (ServiceFlow m r, HasShortDurationRetryCfg r c) => Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> AutoCompleteReq -> m Maps.AutoCompleteResp
autoComplete merchantId merchantOpCityId entityId AutoCompleteReq {..} = do
  merchantCity <- QMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  Maps.autoComplete
    merchantId
    merchantOpCityId
    entityId
    Maps.AutoCompleteReq
      { country = toInterfaceCountry merchantCity.country,
        radiusWithUnit = Just $ fromMaybe (convertMetersToDistance merchantCity.distanceUnit $ fromInteger radius) radiusWithUnit,
        ..
      }
  where
    toInterfaceCountry = \case
      Context.India -> Maps.India
      Context.France -> Maps.France
      Context.USA -> Maps.USA
      Context.Netherlands -> Maps.Netherlands
      Context.Finland -> Maps.Finland
      Context.AnyCountry -> Maps.India
