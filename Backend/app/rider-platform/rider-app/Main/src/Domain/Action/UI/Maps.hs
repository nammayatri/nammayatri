{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Maps
  ( AutoCompleteReq,
    Maps.AutoCompleteResp,
    Maps.GetPlaceDetailsReq,
    Maps.GetPlaceDetailsResp,
    Maps.GetPlaceNameReq,
    Maps.GetPlaceNameResp,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
  )
where

import qualified Data.Geohash as DG
import Data.Text (pack)
import Domain.Types.Maps.PlaceNameCache as DTM
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Maps.Interface.Types as MIT
import Kernel.External.Maps.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Maps.PlaceNameCache as CM
import qualified Storage.CachedQueries.Merchant.MerchantConfigNew as QMCN
import qualified Tools.Maps as Maps

data AutoCompleteReq = AutoCompleteReq
  { input :: Text,
    sessionToken :: Maybe Text,
    location :: Text,
    radius :: Integer,
    language :: Maps.Language,
    strictbounds :: Maybe Bool,
    origin :: Maybe Maps.LatLong
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

autoComplete :: ServiceFlow m r => Id DMerchant.Merchant -> AutoCompleteReq -> m Maps.AutoCompleteResp
autoComplete merchantId AutoCompleteReq {..} = do
  merchantConfig <- QMCN.findByMerchantId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  Maps.autoComplete
    merchantId
    Maps.AutoCompleteReq
      { country = toInterfaceCountry merchantConfig.country,
        ..
      }
  where
    toInterfaceCountry = \case
      Context.India -> Maps.India
      Context.France -> Maps.France
      Context.AnyCountry -> Maps.India

getPlaceDetails :: ServiceFlow m r => (Id DP.Person, Id DMerchant.Merchant) -> Maps.GetPlaceDetailsReq -> m Maps.GetPlaceDetailsResp
getPlaceDetails (_, merchantId) req = do
  Maps.getPlaceDetails merchantId req

getPlaceName :: ServiceFlow m r => (Id DP.Person, Id DMerchant.Merchant) -> Maps.GetPlaceNameReq -> m Maps.GetPlaceNameResp
getPlaceName (_, merchantId) req = do
  merchantConfig <- QMCN.findByMerchantId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  case req.getBy of
    MIT.ByLatLong (Maps.LatLong lat lon) -> do
      let myGeohash = DG.encode merchantConfig.geoHashPrecisionValue (lat, lon)
      case myGeohash of
        Just geoHash -> do
          placeNameCache <- CM.findPlaceByGeoHash (pack geoHash)
          if null placeNameCache
            then callMapsApi merchantId req merchantConfig.geoHashPrecisionValue
            else pure $ map convertToGetPlaceNameResp placeNameCache
        Nothing -> callMapsApi merchantId req merchantConfig.geoHashPrecisionValue
    MIT.ByPlaceId placeId -> do
      placeNameCache <- CM.findPlaceByPlaceId placeId
      if null placeNameCache
        then callMapsApi merchantId req merchantConfig.geoHashPrecisionValue
        else pure $ map convertToGetPlaceNameResp placeNameCache

callMapsApi :: (MonadFlow m, ServiceFlow m r) => Id DMerchant.Merchant -> Maps.GetPlaceNameReq -> Int -> m Maps.GetPlaceNameResp
callMapsApi merchantId req geoHashPrecisionValue = do
  res <- Maps.getPlaceName merchantId req
  let firstElement = listToMaybe res
  case firstElement of
    Just element -> do
      let (latitude, longitude) = case req.getBy of
            MIT.ByLatLong (Maps.LatLong lat lon) -> (lat, lon)
            _ -> (element.location.lat, element.location.lon)
      placeNameCache <- convertResultsRespToPlaceNameCache element latitude longitude geoHashPrecisionValue
      _ <- CM.create placeNameCache
      case (placeNameCache.placeId, placeNameCache.geoHash) of
        (Just placeId, Just geoHash) -> do
          CM.cachedPlaceByPlaceId placeId [placeNameCache]
          CM.cachedPlaceByGeoHash geoHash [placeNameCache]
        (Just placeId, Nothing) -> do
          CM.cachedPlaceByPlaceId placeId [placeNameCache]
        (Nothing, Just geoHash) -> do
          CM.cachedPlaceByGeoHash geoHash [placeNameCache]
        _ -> pure ()
    Nothing -> pure ()
  return res

convertToGetPlaceNameResp :: PlaceNameCache -> Maps.PlaceName
convertToGetPlaceNameResp placeNameCache =
  MIT.PlaceName
    { formattedAddress = placeNameCache.formattedAddress,
      addressComponents = map (\DTM.AddressResp {..} -> MIT.AddressResp {..}) placeNameCache.addressComponents,
      plusCode = placeNameCache.plusCode,
      location = LatLong {lat = placeNameCache.lat, lon = placeNameCache.lon},
      placeId = placeNameCache.placeId
    }

convertResultsRespToPlaceNameCache :: MonadGuid m => MIT.PlaceName -> Double -> Double -> Int -> m DTM.PlaceNameCache
convertResultsRespToPlaceNameCache resultsResp latitude longitude geoHashPrecisionValue = do
  id <- generateGUID
  let res =
        DTM.PlaceNameCache
          { id,
            formattedAddress = resultsResp.formattedAddress,
            addressComponents = map (\MIT.AddressResp {..} -> DTM.AddressResp {..}) resultsResp.addressComponents,
            plusCode = resultsResp.plusCode,
            lat = resultsResp.location.lat,
            lon = resultsResp.location.lon,
            placeId = resultsResp.placeId,
            geoHash = pack <$> DG.encode geoHashPrecisionValue (latitude, longitude)
          }
  return res
