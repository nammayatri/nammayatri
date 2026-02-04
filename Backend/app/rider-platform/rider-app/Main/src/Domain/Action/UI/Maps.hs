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
    makeAutoCompleteKey,
    AutoCompleteType (..),
  )
where

import qualified Data.Geohash as DG
import Data.Text (pack)
import qualified Data.Time as DT
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.PlaceNameCache as DTM
import qualified Kernel.External.Maps.Interface.Types as MIT
import Kernel.External.Maps.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Maps.PlaceNameCache as CM
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.Person as CQP
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps

data AutoCompleteType = PICKUP | DROP deriving (Generic, Show, Read, Eq, Ord, FromJSON, ToJSON, ToSchema)

data AutoCompleteReq = AutoCompleteReq
  { input :: Text,
    sessionToken :: Maybe Text,
    location :: Text,
    radius :: Integer,
    radiusWithUnit :: Maybe Distance,
    language :: Maps.Language,
    types_ :: Maybe Text,
    strictbounds :: Maybe Bool,
    origin :: Maybe Maps.LatLong,
    autoCompleteType :: Maybe AutoCompleteType
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

makeAutoCompleteKey :: Text -> Text -> Text
makeAutoCompleteKey token typeOfSearch = "Analytics-RiderApp-AutoComplete-Data" <> token <> "|" <> typeOfSearch

autoComplete :: (ServiceFlow m r, EventStreamFlow m r, HasShortDurationRetryCfg r c) => (Id DP.Person, Id DMerchant.Merchant) -> Maybe Text -> AutoCompleteReq -> m Maps.AutoCompleteResp
autoComplete (personId, merchantId) entityId AutoCompleteReq {..} = do
  merchantOperatingCityId <- CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)
  merchantOperatingCity <- QMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  fork "Inserting/Updating autocomplete data" $ do
    riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, txnId = Nothing})
    whenJust riderConfig $ \config -> do
      let toCollectData = fromMaybe False config.collectAutoCompleteData
      when toCollectData $ do
        let autoCompleteDataCollectionCondition = (isJust sessionToken) && (isJust autoCompleteType)
        when autoCompleteDataCollectionCondition $ do
          let reqOrigin = fromMaybe (Maps.LatLong 0 0) origin
          let token = fromMaybe "" sessionToken
          let typeOfSearch = fromMaybe DROP autoCompleteType
          let key = makeAutoCompleteKey token (show typeOfSearch)
          currentRecord :: Maybe AutoCompleteEventData <- Redis.safeGet key
          now <- getCurrentTime
          case currentRecord of
            Just record -> do
              let currentSting = record.autocompleteInputs
              let updatedRecord = AutoCompleteEventData (currentSting <> "|" <> input) record.customerId record.id record.isLocationSelectedOnMap record.searchRequestId record.searchType record.sessionToken record.merchantId record.merchantOperatingCityId record.originLat record.originLon record.createdAt now
              triggerAutoCompleteEvent updatedRecord
              Redis.setExp key updatedRecord 300
            Nothing -> do
              uid <- generateGUID
              let autoCompleteData = AutoCompleteEventData input personId uid Nothing Nothing (show typeOfSearch) token merchantId merchantOperatingCityId (show reqOrigin.lat) (show reqOrigin.lon) now now
              triggerAutoCompleteEvent autoCompleteData
              Redis.setExp key autoCompleteData 300
  Maps.autoComplete
    merchantId
    merchantOperatingCityId
    entityId
    Maps.AutoCompleteReq
      { country = toInterfaceCountry merchantOperatingCity.country,
        radiusWithUnit = Just $ fromMaybe (convertMetersToDistance merchantOperatingCity.distanceUnit $ fromInteger radius) radiusWithUnit,
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

getPlaceDetails :: ServiceFlow m r => (Id DP.Person, Id DMerchant.Merchant) -> Maybe Text -> Maps.GetPlaceDetailsReq -> m Maps.GetPlaceDetailsResp
getPlaceDetails (personId, merchantId) entityId req = do
  merchantOperatingCityId <- CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)
  Maps.getPlaceDetails merchantId merchantOperatingCityId entityId req

expirePlaceNameCache :: ServiceFlow m r => [PlaceNameCache] -> Id DMOC.MerchantOperatingCity -> m ()
expirePlaceNameCache placeNameCache merchantOperatingCityId = do
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  whenJust riderConfig.placeNameCacheExpiryDays $ \cacheExpiry -> do
    currentTime <- liftIO DT.getCurrentTime
    let expiryDate = DT.addUTCTime (DT.nominalDay * fromIntegral (- cacheExpiry)) currentTime
    let toBeDeletedPlaceNameCache = filter (\obj -> obj.createdAt < expiryDate) placeNameCache
    mapM_ CM.delete toBeDeletedPlaceNameCache

getPlaceName :: ServiceFlow m r => (Id DP.Person, Id DMerchant.Merchant) -> Maybe Text -> Maps.GetPlaceNameReq -> m Maps.GetPlaceNameResp
getPlaceName (personId, merchantId) entityId req = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOperatingCityId <- CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)
  case req.getBy of
    MIT.ByLatLong (Maps.LatLong lat lon) -> do
      let myGeohash = DG.encode merchant.geoHashPrecisionValue (lat, lon)
      case myGeohash of
        Just geoHash -> do
          placeNameCache' <- CM.findPlaceByGeoHash (pack geoHash)
          let placeNameCache = fst placeNameCache'
              source = snd placeNameCache'
          fork "Place Name Cache Expiry" $ expirePlaceNameCache placeNameCache merchantOperatingCityId
          if null placeNameCache
            then callMapsApi merchantId merchantOperatingCityId entityId req merchant.geoHashPrecisionValue
            else pure $ map (convertToGetPlaceNameResp source) placeNameCache
        Nothing -> callMapsApi merchantId merchantOperatingCityId entityId req merchant.geoHashPrecisionValue
    MIT.ByPlaceId placeId -> do
      placeNameCache' <- CM.findPlaceByPlaceId placeId
      let placeNameCache = fst placeNameCache'
          source = snd placeNameCache'
      fork "Place Name Cache Expiry" $ expirePlaceNameCache placeNameCache merchantOperatingCityId
      if null placeNameCache
        then callMapsApi merchantId merchantOperatingCityId entityId req merchant.geoHashPrecisionValue
        else pure $ map (convertToGetPlaceNameResp source) placeNameCache

callMapsApi :: (MonadFlow m, ServiceFlow m r, HasKafkaProducer r) => Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maps.GetPlaceNameReq -> Int -> m Maps.GetPlaceNameResp
callMapsApi merchantId merchantOperatingCityId entityId req geoHashPrecisionValue = do
  res <- Maps.getPlaceName merchantId merchantOperatingCityId entityId req
  let firstElement = listToMaybe res
  case firstElement of
    Just element -> do
      let (latitude, longitude) = case req.getBy of
            MIT.ByLatLong (Maps.LatLong lat lon) -> (lat, lon)
            _ -> (element.location.lat, element.location.lon)
      placeNameCache <- convertResultsRespToPlaceNameCache element latitude longitude geoHashPrecisionValue
      _ <- CM.create placeNameCache
      whenJust placeNameCache.placeId $ \placeid -> do
        CM.cachedPlaceByPlaceId placeid [placeNameCache]
      whenJust placeNameCache.geoHash $ \geohash -> do
        CM.cachedPlaceByGeoHash geohash [placeNameCache]
    Nothing -> pure ()
  return (map (\MIT.PlaceName {..} -> MIT.PlaceName {source = (Just . pack . show) Google, ..}) res)

convertToGetPlaceNameResp :: CM.Source -> PlaceNameCache -> Maps.PlaceName
convertToGetPlaceNameResp source placeNameCache =
  MIT.PlaceName
    { formattedAddress = placeNameCache.formattedAddress,
      addressComponents = map (\DTM.AddressResp {..} -> MIT.AddressResp {..}) placeNameCache.addressComponents,
      plusCode = placeNameCache.plusCode,
      location = LatLong {lat = placeNameCache.lat, lon = placeNameCache.lon},
      placeId = placeNameCache.placeId,
      source = (Just . pack . show) source
    }

convertResultsRespToPlaceNameCache :: (MonadTime m, MonadGuid m) => MIT.PlaceName -> Double -> Double -> Int -> m DTM.PlaceNameCache
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
